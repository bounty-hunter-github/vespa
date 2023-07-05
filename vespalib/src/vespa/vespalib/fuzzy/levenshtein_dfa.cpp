// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.
#include "explicit_levenshtein_dfa.h"
#include "implicit_levenshtein_dfa.h"
#include "levenshtein_dfa.h"
#include <vespa/vespalib/util/stringfmt.h>
#include <memory>

namespace vespalib::fuzzy {

LevenshteinDfa::LevenshteinDfa(std::unique_ptr<Impl> impl) noexcept
    : _impl(std::move(impl))
{}

LevenshteinDfa::LevenshteinDfa(LevenshteinDfa&&) noexcept = default;
LevenshteinDfa& LevenshteinDfa::operator=(LevenshteinDfa&&) noexcept = default;

LevenshteinDfa::~LevenshteinDfa() = default;

LevenshteinDfa::MatchResult
LevenshteinDfa::match(std::string_view u8str, std::string* successor_out) const {
    return _impl->match(u8str, successor_out);
}

size_t LevenshteinDfa::memory_usage() const noexcept {
    return _impl->memory_usage();
}

void LevenshteinDfa::dump_as_graphviz(std::ostream& out) const {
    _impl->dump_as_graphviz(out);
}

LevenshteinDfa LevenshteinDfa::build(std::string_view target_string, uint8_t max_edits, DfaType dfa_type) {
    if (max_edits != 1 && max_edits != 2) {
        throw std::invalid_argument(make_string("Levenshtein DFA max_edits must be in {1, 2}, was %u", max_edits));
    }
    if (dfa_type == DfaType::Implicit) {
        if (max_edits == 1) {
            return LevenshteinDfa(std::make_unique<ImplicitLevenshteinDfa<FixedMaxEditDistanceTraits<1>>>(target_string));
        } else { // max_edits == 2
            return LevenshteinDfa(std::make_unique<ImplicitLevenshteinDfa<FixedMaxEditDistanceTraits<2>>>(target_string));
        }
    } else { // DfaType::Explicit
        if (max_edits == 1) {
            return ExplicitLevenshteinDfaBuilder<FixedMaxEditDistanceTraits<1>>(target_string).build_dfa();
        } else { // max_edits == 2
            return ExplicitLevenshteinDfaBuilder<FixedMaxEditDistanceTraits<2>>(target_string).build_dfa();
        }
    }

}

LevenshteinDfa LevenshteinDfa::build(std::string_view target_string, uint8_t max_edits) {
    // TODO measure sweet spot for explicit vs implicit and auto-choose depending on target length?
    return build(target_string, max_edits, DfaType::Implicit);
}

std::ostream& operator<<(std::ostream& os, const LevenshteinDfa::MatchResult& mos) {
    if (mos.matches()) {
        os << "match(" << static_cast<int>(mos.edits()) << " edits)";
    } else {
        os << "mismatch";
    }
    return os;
}

}
