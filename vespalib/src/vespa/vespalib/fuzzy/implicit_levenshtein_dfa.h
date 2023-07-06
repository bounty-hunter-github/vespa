// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
#pragma once

#include "levenshtein_dfa.h"
#include "unicode_utils.h"

namespace vespalib::fuzzy {

template <typename Traits>
class ImplicitLevenshteinDfa final : public LevenshteinDfa::Impl {
    std::u32string _u32_str_buf;
public:
    using MatchResult = LevenshteinDfa::MatchResult;

    explicit ImplicitLevenshteinDfa(std::u8string_view str)
        : ImplicitLevenshteinDfa(utf8_string_to_utf32(str))
    {}

    explicit ImplicitLevenshteinDfa(std::string_view str)
        : ImplicitLevenshteinDfa(utf8_string_to_utf32(str))
    {}

    explicit ImplicitLevenshteinDfa(std::u32string str) noexcept
        : _u32_str_buf(std::move(str))
    {}

    ~ImplicitLevenshteinDfa() override = default;

    [[nodiscard]] MatchResult match(std::string_view u8str, std::string* successor_out) const override;

    [[nodiscard]] size_t memory_usage() const noexcept override {
        return _u32_str_buf.size() * sizeof(uint32_t);
    }

    void dump_as_graphviz(std::ostream& os) const override;
};

}
