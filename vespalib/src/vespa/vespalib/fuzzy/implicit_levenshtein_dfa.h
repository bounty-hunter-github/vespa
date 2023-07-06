// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
#pragma once

#include "dfa_stepping_base.h"
#include "levenshtein_dfa.h"
#include "sparse_state.h"

namespace vespalib::fuzzy {

template <typename Traits>
class ImplicitLevenshteinDfa
    : public DfaSteppingBase<Traits>,
      public LevenshteinDfa::Impl
{
    using Base = DfaSteppingBase<Traits>;

    using StateType   = typename Base::StateType;
    using MatchResult = LevenshteinDfa::MatchResult;

    using Base::_u32_str;
    using Base::max_edits;
    using Base::start;
    using Base::match_edit_distance;
    using Base::step;
    using Base::can_wildcard_step;
    using Base::is_match;
    using Base::can_match;
public:
    explicit ImplicitLevenshteinDfa(std::u8string_view str)
        : ImplicitLevenshteinDfa(utf8_string_to_utf32(str))
    {}

    explicit ImplicitLevenshteinDfa(std::string_view str)
        : ImplicitLevenshteinDfa(utf8_string_to_utf32(str))
    {}

    ImplicitLevenshteinDfa(std::u32string str) noexcept
        : DfaSteppingBase<Traits>(std::move(str))
    {
    }

    ~ImplicitLevenshteinDfa() override = default;

    bool has_wildcard_out(const StateType& state) const noexcept {
        return can_match(step(state, UINT32_MAX));
    }

    template <typename F>
    bool has_any_char_matching(const StateType& state, F&& f) const noexcept(noexcept(f(uint32_t{}))) {
        for (uint32_t i = 0; i < state.size(); ++i) {
            const auto idx = state.index(i);
            if ((idx < _u32_str.size()) && f(_u32_str[idx])) {
                return true;
            }
        }
        return false;
    }

    template <typename F>
    void for_each_char(const StateType& state, F&& f) const noexcept(noexcept(f(uint32_t{}))) {
        for (uint32_t i = 0; i < state.size(); ++i) {
            const auto idx = state.index(i);
            if ((idx < _u32_str.size())) [[likely]] {
                f(_u32_str[idx]);
            }
        }
    }

    bool has_explicit_higher_out_edge(const StateType& state, uint32_t ch) const noexcept;
    bool has_exact_match(const StateType& state, uint32_t ch) const noexcept;
    uint32_t smallest_out_char(const StateType& state) const noexcept ;
    uint32_t lowest_higher_explicit_out_char(const StateType& state, uint32_t ch) const;

    void emit_smallest_matching_suffix(const StateType& from, std::string& str) const;
    void backtrack_and_emit_greater_suffix(const StateType& last_state_with_higher_out,
                                           const uint32_t input_at_branch,
                                           std::string& successor) const;

    [[nodiscard]] LevenshteinDfa::MatchResult match(std::string_view u8str, std::string* successor_out) const override;

    [[nodiscard]] size_t memory_usage() const noexcept override {
        return _u32_str.size() * sizeof(uint32_t);
    }

    void dump_as_graphviz(std::ostream& os) const override;
};

}
