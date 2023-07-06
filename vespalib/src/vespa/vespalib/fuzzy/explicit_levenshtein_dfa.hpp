// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
#pragma once

#include "explicit_levenshtein_dfa.h"
#include "unicode_utils.h"
#include <vespa/vespalib/text/utf8.h>
#include <vespa/vespalib/stllike/hash_map.h>
#include <vespa/vespalib/stllike/hash_map.hpp>
#include <iostream>
#include <queue>

namespace vespalib::fuzzy {

template <uint8_t MaxEdits>
void ExplicitLevenshteinDfaImpl<MaxEdits>::emit_smallest_matching_suffix(const DfaNodeType& from, std::string& str) const {
    const auto* node = &from;
    while (node->edits > max_edits()) {
        // if we can take a wildcard path, emit the smallest possible valid utf-8 character (0x01)
        // otherwise, find the smallest char that can eventually lead us to a match.
        if (node->has_wildcard_edge()) {
            str += '\x01';
            assert(node->wildcard_edge_to < _nodes.size());
            node = &_nodes[node->wildcard_edge_to];
        } else {
            assert(!node->match_out_edges().empty());
            // Out-edges are pre-ordered in increasing code point order, so the first element
            // is always the smallest possible matching character.
            const auto& smallest_out = node->match_out_edges().front();
            assert(smallest_out.node < _nodes.size());
            append_utf32_char_as_utf8(str, smallest_out.u32ch);
            node = &_nodes[smallest_out.node];
        }
    }
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
template <uint8_t MaxEdits>
void ExplicitLevenshteinDfaImpl<MaxEdits>::backtrack_and_emit_greater_suffix(
        const DfaNodeType* last_node_with_higher_out,
        const uint32_t input_at_branch,
        std::string& successor) const
{
    if (last_node_with_higher_out->has_wildcard_edge()) {
        // `input_at_branch` may be U+10FFFF, with +1 being outside legal Unicode _code point_
        // range but _within_ what UTF-8 can technically _encode_.
        // We assume that successor-consumers do not care about anything except byte-wise ordering.
        // This is similar to what RE2's PossibleMatchRange emits to represent a UTF-8 upper bound,
        // so not without precedent.
        const auto next_char = input_at_branch + 1;
        if (!last_node_with_higher_out->has_exact_match(next_char)) {
            append_utf32_char_as_utf8(successor, next_char);
            emit_smallest_matching_suffix(_nodes[last_node_with_higher_out->wildcard_edge_to], successor);
            return;
        } // else: handle exact match below (it will be found as the first higher out edge)
    }
    const auto* first_highest = last_node_with_higher_out->lowest_higher_explicit_out_edge(input_at_branch);
    assert(first_highest != nullptr);
    append_utf32_char_as_utf8(successor, first_highest->u32ch);
    emit_smallest_matching_suffix(_nodes[first_highest->node], successor);
}

template <uint8_t MaxEdits>
LevenshteinDfa::MatchResult
ExplicitLevenshteinDfaImpl<MaxEdits>::match(std::string_view u8str, std::string* successor_out) const {
    vespalib::Utf8Reader u8_reader(u8str.data(), u8str.size());
    const DfaNodeType* node = &_nodes[0];
    const DfaNodeType* last_node_with_higher_out = nullptr;
    uint32_t n_prefix_u8_bytes = 0;
    uint32_t char_after_prefix = 0;
    while (u8_reader.hasMore()) {
        const auto u8_pos_before_char = u8_reader.getPos();
        const uint32_t mch = u8_reader.getChar();
        if (node->has_higher_out_edge(mch)) {
            last_node_with_higher_out = node;
            n_prefix_u8_bytes = u8_pos_before_char;
            char_after_prefix = mch;
        }
        auto maybe_next = node->match_or_doomed(mch);
        if (maybe_next != DOOMED) {
            assert(maybe_next < _nodes.size());
            node = &_nodes[maybe_next];
        } else {
            // Can never match; find the successor if requested
            if (successor_out) {
                *successor_out = u8str.substr(0, n_prefix_u8_bytes);
                assert(last_node_with_higher_out != nullptr);
                backtrack_and_emit_greater_suffix(last_node_with_higher_out, char_after_prefix, *successor_out);
            }
            return MatchResult::make_mismatch(max_edits());
        }
    }
    if (node->edits <= max_edits()) {
        return MatchResult::make_match(max_edits(), node->edits);
    }
    // we've matched the entire input string without ending in an accepting state. This can only
    // happen if the input is a (possibly edited) prefix of the target string. Any and all _longer_
    // strings with this prefix is inherently lexicographically greater, so emit the smallest
    // possible suffix that turns prefix || suffix into a matching string.
    if (successor_out) {
        *successor_out = u8str;
        emit_smallest_matching_suffix(*node, *successor_out);
    }
    return MatchResult::make_mismatch(max_edits());
}

template <uint8_t MaxEdits>
void ExplicitLevenshteinDfaImpl<MaxEdits>::dump_as_graphviz(std::ostream& os) const {
    os << std::dec << "digraph levenshtein_dfa {\n";
    os << "    fontname=\"Helvetica,Arial,sans-serif\"\n";
    os << "    node [shape=circle, fontname=\"Helvetica,Arial,sans-serif\", fixedsize=true];\n";
    os << "    edge [fontname=\"Helvetica,Arial,sans-serif\"];\n";
    for (size_t i = 0; i < _nodes.size(); ++i) {
        const auto& node = _nodes[i];
        if (node.edits <= max_edits()) {
            os << "    " << i << " [label=\"" << i << "(" << static_cast<int>(node.edits) << ")\", style=\"filled\"];\n";
        }
        for (const auto& edge : node.match_out_edges()) {
            std::string as_utf8;
            append_utf32_char_as_utf8(as_utf8, edge.u32ch);
            os << "    " << i << " -> " << edge.node << " [label=\"" << as_utf8 << "\"];\n";
        }
        if (node.wildcard_edge_to != DOOMED) {
            os << "    " << i << " -> " << node.wildcard_edge_to << " [label=\"*\"];\n";
        }
    }
    os << "}\n";
}

namespace {

template <typename StateType>
struct ExploreState {
    using NodeIdAndExplored    = std::pair<uint32_t, bool>;
    using SparseExploredStates = vespalib::hash_map<StateType, NodeIdAndExplored, typename StateType::hash>;

    uint32_t             state_counter;
    SparseExploredStates explored_states;

    ExploreState();
    ~ExploreState();

    [[nodiscard]] SparseExploredStates::iterator node_of(const StateType& state) {
        auto maybe_explored = explored_states.find(state);
        if (maybe_explored != explored_states.end()) {
            return maybe_explored;
        }
        uint32_t this_node = state_counter;
        assert(state_counter < UINT32_MAX);
        ++state_counter;
        return explored_states.insert(std::make_pair(state, std::make_pair(this_node, false))).first; // not yet explored;
    }

    [[nodiscard]] bool already_explored(const SparseExploredStates::iterator& node) const noexcept {
        return node->second.second;
    }

    void tag_as_explored(SparseExploredStates::iterator& node) noexcept {
        node->second.second = true;
    }
};

template <typename StateType>
ExploreState<StateType>::ExploreState()
    : state_counter(0),
      explored_states()
{}

template <typename StateType>
ExploreState<StateType>::~ExploreState() = default;

} // anon ns

template <typename Traits>
LevenshteinDfa ExplicitLevenshteinDfaBuilder<Traits>::build_dfa() const {
    auto dfa = std::make_unique<ExplicitLevenshteinDfaImpl<max_edits()>>();
    ExploreState<StateType> exp;
    // Use BFS instead of DFS to ensure most node edges point to nodes that are allocated _after_
    // the parent node, which means the CPU can skip ahead instead of ping-ponging back and forth.
    // This does _not_ always hold, such as if you have A->B and A->C->B (i.e. both parent and
    // grandparent have a transition to the same state), in which case B may be allocated before C.
    std::queue<StateType> to_explore;
    to_explore.push(start());
    while (!to_explore.empty()) {
        auto state = std::move(to_explore.front());
        to_explore.pop();
        auto this_node = exp.node_of(state); // note: invalidated by subsequent calls to node_of
        if (exp.already_explored(this_node)) {
            continue;
        }
        exp.tag_as_explored(this_node);
        const auto this_node_idx = this_node->second.first;
        dfa->ensure_node_array_large_enough_for_index(this_node_idx);
        dfa->set_node_edit_distance(this_node_idx, match_edit_distance(state));
        auto t = transitions(state);
        for (uint32_t out_c : t.u32_chars()) {
            auto new_state = step(state, out_c);
            auto out_node = exp.node_of(new_state);
            dfa->add_outgoing_edge(this_node_idx, out_node->second.first, out_c);
            to_explore.push(std::move(new_state));
        }
        auto wildcard_state = step(state, UINT32_MAX); // never-matching sentinel
        if (can_match(wildcard_state)) {
            auto out_node = exp.node_of(wildcard_state);
            dfa->set_wildcard_edge(this_node_idx, out_node->second.first);
            to_explore.push(std::move(wildcard_state));
        } // else: don't bother
    }
    return LevenshteinDfa(std::move(dfa));
}

}
