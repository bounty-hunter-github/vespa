// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
#pragma once

#include "implicit_levenshtein_dfa.h"
#include "unicode_utils.h"
#include <vespa/vespalib/text/utf8.h>
#include <cassert>
#include <stdexcept>

namespace vespalib::fuzzy {

template <typename Traits>
bool ImplicitLevenshteinDfa<Traits>::has_explicit_higher_out_edge(const StateType& state, uint32_t ch) const noexcept {
    return has_any_char_matching(state, [ch](uint32_t state_ch) noexcept {
        return state_ch > ch;
    });
}

template <typename Traits>
bool ImplicitLevenshteinDfa<Traits>::has_exact_match(const StateType& state, uint32_t ch) const noexcept {
    return has_any_char_matching(state, [ch](uint32_t state_ch) noexcept {
        return state_ch == ch;
    });
}

template <typename Traits>
uint32_t ImplicitLevenshteinDfa<Traits>::smallest_out_char(const StateType& state) const noexcept {
    assert(!state.empty());
    uint32_t min_ch = UINT32_MAX;
    for_each_char(state, [&min_ch](uint32_t state_ch) noexcept {
        min_ch = std::min(min_ch, state_ch);
    });
    return min_ch;
}

template <typename Traits>
uint32_t ImplicitLevenshteinDfa<Traits>::lowest_higher_explicit_out_char(const StateType& state, uint32_t ch) const {
    assert(!state.empty());
    uint32_t min_ch = UINT32_MAX;
    for_each_char(state, [ch, &min_ch](uint32_t state_ch) noexcept {
        if ((state_ch > ch) && (state_ch < min_ch)) {
            min_ch = state_ch;
        }
    });
    return min_ch;
}

template <typename Traits>
void ImplicitLevenshteinDfa<Traits>::emit_smallest_matching_suffix(const StateType& from, std::string& str) const {
    auto state = from;
    while (!is_match(state)) {
        // if we can take a wildcard path, emit the smallest possible valid utf-8 character (0x01)
        // otherwise, find the smallest char that can eventually lead us to a match.
        auto wildcard_state = step(state, UINT32_MAX);
        if (can_match(wildcard_state)) {
            str += '\x01';
            state = wildcard_state;
        } else {
            const uint32_t smallest_out = smallest_out_char(state);
            append_utf32_char_as_utf8(str, smallest_out);
            state = step(state, smallest_out);
        }
    }
}

template <typename Traits>
void ImplicitLevenshteinDfa<Traits>::backtrack_and_emit_greater_suffix(
        const StateType& last_state_with_higher_out,
        const uint32_t input_at_branch,
        std::string& successor) const
{
    auto wildcard_state = step(last_state_with_higher_out, UINT32_MAX);
    if (can_match(wildcard_state)) {
        // `input_at_branch` may be U+10FFFF, with +1 being outside legal Unicode _code point_
        // range but _within_ what UTF-8 can technically _encode_.
        // We assume that successor-consumers do not care about anything except byte-wise ordering.
        // This is similar to what RE2's PossibleMatchRange emits to represent a UTF-8 upper bound,
        // so not without precedent.
        const auto next_char = input_at_branch + 1;
        if (!has_exact_match(last_state_with_higher_out, next_char)) {
            append_utf32_char_as_utf8(successor, next_char);
            emit_smallest_matching_suffix(wildcard_state, successor);
            return;
        } // else: handle exact match below (it will be found as the first higher out edge)
    }
    const auto lowest_higher_ch = lowest_higher_explicit_out_char(last_state_with_higher_out, input_at_branch);
    assert(lowest_higher_ch != UINT32_MAX);
    append_utf32_char_as_utf8(successor, lowest_higher_ch);
    emit_smallest_matching_suffix(step(last_state_with_higher_out, lowest_higher_ch), successor);
}

template <typename Traits>
LevenshteinDfa::MatchResult
ImplicitLevenshteinDfa<Traits>::match(std::string_view u8str, std::string* successor_out) const {
    vespalib::Utf8Reader u8_reader(u8str.data(), u8str.size());
    uint32_t n_prefix_u8_bytes = 0;
    uint32_t char_after_prefix = 0;
    StateType last_state_with_higher_out;
    auto state = start();
    while (u8_reader.hasMore()) {
        const auto u8_pos_before_char = u8_reader.getPos();
        const uint32_t mch = u8_reader.getChar();
        if (successor_out && (has_explicit_higher_out_edge(state, mch) || can_wildcard_step(state))) {
            last_state_with_higher_out = state;
            n_prefix_u8_bytes = u8_pos_before_char;
            char_after_prefix = mch;
        }
        auto next_state = step(state, mch);
        if (can_match(next_state)) {
            state = next_state;
        } else {
            if (successor_out) {
                *successor_out = u8str.substr(0, n_prefix_u8_bytes);
                assert(!last_state_with_higher_out.empty());
                backtrack_and_emit_greater_suffix(last_state_with_higher_out, char_after_prefix, *successor_out);
            }
            return MatchResult::make_mismatch(max_edits());
        }
    }
    const auto edits = match_edit_distance(state);
    if (edits <= max_edits()) {
        return MatchResult::make_match(max_edits(), edits);
    }
    if (successor_out) {
        *successor_out = u8str;
        emit_smallest_matching_suffix(state, *successor_out);
    }
    return MatchResult::make_mismatch(max_edits());
}

template <typename Traits>
void ImplicitLevenshteinDfa<Traits>::dump_as_graphviz(std::ostream&) const {
    throw std::runtime_error("Graphviz output not available for implicit Levenshtein DFA");
}

}
