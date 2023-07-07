// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
#pragma once

#include "dfa_matcher.h"
#include "levenshtein_dfa.h"
#include "unicode_utils.h"
#include <vespa/vespalib/text/utf8.h>
#include <cassert>
#include <concepts>

namespace vespalib::fuzzy {

/**
 * Implementation of algorithm for linear-time k-max edits string matching and successor
 * string generation over an abstract DFA representation.
 *
 * The implementation is agnostic to how the underlying DFA is implemented, but requires
 * an appropriate adapter that satisfies the DfaMatcher concept contracts.
 */
template <uint8_t MaxEdits>
struct MatchAlgorithm {
    using MatchResult = LevenshteinDfa::MatchResult;

    static constexpr uint8_t max_edits() noexcept { return MaxEdits; }

    /**
     * Matches source string u8str against the target DFA, optionally generating the
     * successor string iff the source string is not within the maximum number of edits
     * of the target string.
     *
     * The actual match loop is very simple: we try to match the DFA as far as we can
     * before either consuming all input (source string) characters or ending up in
     * a non-matching state before we have consumed all input. In the former case,
     * we may be in a matching state (consider matching "car" with the target string
     * "cart"; after consuming all input we'll be in a matching state with 1 edit).
     * In the latter case, the input string cannot possible match.
     *
     * If we end up in a matching state, all is well. We simply return a MatchResult
     * with the number of edits the state represents.
     *
     * The interesting bit happens the string does _not_ match and we are asked to
     * provide a _successor_ string that _does_ match and is strictly greater in
     * lexicographic order.
     *
     * TODO more doc
     *
     * Both the input and successor output strings are in UTF-8 format. To avoid
     * doing duplicate work, we keep track of the byte length of the string prefix
     * that will be part of the successor and simply copy it verbatim instead
     * of building the string from converted UTF-32 -> UTF-8 chars as we go.
     *
     * TODO we could probably also optimize the smallest suffix generation with
     *   this when we know we can no longer insert any smaller char substitutions
     *   and the only way to complete the string is to emit it verbatim.
     *
     */
    template <DfaMatcher Matcher>
    static MatchResult match(const Matcher& matcher,
                             std::string_view u8str,
                             std::string* successor_out)
    {
        using StateType = typename Matcher::StateType;
        vespalib::Utf8Reader u8_reader(u8str.data(), u8str.size());
        uint32_t n_prefix_u8_bytes = 0;
        uint32_t char_after_prefix = 0;
        StateType last_state_with_higher_out = StateType{};

        StateType state = matcher.start();
        while (u8_reader.hasMore()) {
            const auto u8_pos_before_char = u8_reader.getPos();
            const uint32_t mch = u8_reader.getChar();
            if (successor_out && matcher.has_higher_out_edge(state, mch)) {
                last_state_with_higher_out = state;
                n_prefix_u8_bytes = u8_pos_before_char;
                char_after_prefix = mch;
            }
            auto maybe_next = matcher.match_input(state, mch);
            if (matcher.can_match(maybe_next)) {
                state = maybe_next;
            } else {
                // Can never match; find the successor if requested
                if (successor_out) {
                    *successor_out = u8str.substr(0, n_prefix_u8_bytes);
                    assert(matcher.valid_state(last_state_with_higher_out));
                    backtrack_and_emit_greater_suffix(matcher, last_state_with_higher_out,
                                                      char_after_prefix, *successor_out);
                }
                return MatchResult::make_mismatch(max_edits());
            }
        }
        const auto edits = matcher.match_edit_distance(state);
        if (edits <= max_edits()) {
            return MatchResult::make_match(max_edits(), edits);
        }
        // we've matched the entire input string without ending in an accepting state. This can only
        // happen if the input is a (possibly edited) prefix of the target string. Any and all _longer_
        // strings with this prefix is inherently lexicographically greater, so emit the smallest
        // possible suffix that turns prefix || suffix into a matching string.
        if (successor_out) {
            *successor_out = u8str;
            emit_smallest_matching_suffix(matcher, state, *successor_out);
        }
        return MatchResult::make_mismatch(max_edits());
    }

    /**
     * Instantly backtrack to the first possible branching point in the DFA where we can
     * choose some higher outgoing edge character value and still match the DFA. If the node
     * has a wildcard edge, we can bump the input char by one and generate the smallest
     * possible matching suffix to that. Otherwise, choose the smallest out edge that
     * is greater than the input character at that location and _then_ emit the smallest
     * matching prefix.
     *
     * precondition: `last_node_with_higher_out` has either a wildcard edge or a char match
     *    edge that compares greater than `input_at_branch`.
     */
    template <DfaMatcher Matcher>
    static void backtrack_and_emit_greater_suffix(
            const Matcher& matcher,
            typename Matcher::StateParamType last_state_with_higher_out,
            const uint32_t input_at_branch,
            std::string& successor)
    {
        auto wildcard_state = matcher.match_wildcard(last_state_with_higher_out);
        if (matcher.can_match(wildcard_state)) {
            // `input_at_branch` may be U+10FFFF, with +1 being outside legal Unicode _code point_
            // range but _within_ what UTF-8 can technically _encode_.
            // We assume that successor-consumers do not care about anything except byte-wise ordering.
            // This is similar to what RE2's PossibleMatchRange emits to represent a UTF-8 upper bound,
            // so not without precedent.
            const auto next_char = input_at_branch + 1;
            if (!matcher.has_exact_explicit_out_edge(last_state_with_higher_out, next_char)) {
                append_utf32_char_as_utf8(successor, next_char);
                emit_smallest_matching_suffix(matcher, wildcard_state, successor);
                return;
            } // else: handle exact match below (it will be found as the first higher out edge)
        }
        const auto first_highest_edge = matcher.lowest_higher_explicit_out_edge(last_state_with_higher_out, input_at_branch);
        assert(matcher.valid_edge(first_highest_edge));
        append_utf32_char_as_utf8(successor, matcher.edge_to_u32char(first_highest_edge));
        emit_smallest_matching_suffix(matcher, matcher.edge_to_state(last_state_with_higher_out, first_highest_edge), successor);
    }

    /**
     * TODO doc
     */
    template <DfaMatcher Matcher>
    static void emit_smallest_matching_suffix(
            const Matcher& matcher,
            typename Matcher::StateParamType from,
            std::string& str)
    {
        auto state = from;
        while (!matcher.is_match(state)) {
            // If we can take a wildcard path, emit the smallest possible valid UTF-8 character (0x01).
            // Otherwise, find the smallest char that can eventually lead us to a match.
            auto wildcard_state = matcher.match_wildcard(state);
            if (matcher.can_match(wildcard_state)) {
                str += '\x01';
                state = wildcard_state;
            } else {
                const auto smallest_out_edge = matcher.smallest_explicit_out_edge(state);
                assert(matcher.valid_edge(smallest_out_edge));
                append_utf32_char_as_utf8(str, matcher.edge_to_u32char(smallest_out_edge));
                state = matcher.edge_to_state(state, smallest_out_edge);
            }
        }
    }
};

}
