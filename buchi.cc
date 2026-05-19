#include <cassert>
#include <cstddef>

#include <algorithm>
#include <memory>
#include <string>
#include <vector>
#include <set>
#include <unordered_set>

#include <iostream>

enum Operator {
    TRUE,
    FALSE,
    ATOM,
    UNARY_FIRST,
    NOT = UNARY_FIRST,
    X,
    F,
    G,
    BINARY_FIRST,
    AND = BINARY_FIRST,
    OR,
    IMPL,
    U,
    W,
    R
};

static const struct {
    char sym, opc;
} opcodes[] = {
    { '!', NOT },
    { '&', AND },
    { '|', OR },
    { 'U', U },
    { 'F', F },
    { 'G', G },
    { 'R', R },
    { 'W', W },
    { 'X', X }
};

static char
symbol_of(Operator opc) {
    for (const auto &item: opcodes) {
        if (item.opc == opc) {
            return item.sym;
        }
    }
    return '\0';
}

static Operator
opcode_of(char c) {
    for (const auto &item: opcodes) {
        if (item.sym == c) {
            return static_cast<Operator>(item.opc);
        }
    }
    return FALSE;
}

class Ltl {
public:
    static const Ltl * True() {
        static Ltl *ltl_true = new Ltl(TRUE);
        ltl_true->build_presentation_string();
        return ltl_true;
    }

    static const Ltl * False() {
        static Ltl *ltl_false = new Ltl(FALSE);
        ltl_false->build_presentation_string();
        return ltl_false;
    }

    static const Ltl * atom(std::string name) {
        Ltl *ltl = new Ltl(std::move(name));
        ltl->build_presentation_string();
        return ltl;
    }

    static const Ltl * unary(Operator opc, const Ltl &opnd) {
        Ltl *ltl = new Ltl(opc);
        ltl->lop = &opnd;
        ltl->build_presentation_string();

        return ltl;
    }

    static const Ltl * binary(Operator opc, const Ltl &lop, const Ltl &rop) {
        Ltl *ltl = new Ltl(opc);
        ltl->lop = &lop;
        ltl->rop = &rop;
        ltl->build_presentation_string();

        return ltl;
    }

    Operator kind() const {
        return opc;
    }

    const Ltl *lhs() const {
        return lop;
    }

    const Ltl *rhs() const {
        return rop;
    }

    void to_string(std::string &s) const {
        s = presentation;
    }

    std::string get_presentation() const {
        return presentation;
    }

    void build_presentation_string() {
        if (!presentation.length()) build_presentation_string(presentation);
    }

    bool operator==(const Ltl& other) const noexcept {
        return presentation == other.presentation;
    }

private:
    Ltl(std::string _name) {
        opc = ATOM;
        name = std::move(_name);
        lop = nullptr;
        rop = nullptr;
    }

    Ltl(Operator _opc) {
        opc = _opc;
        lop = nullptr;
        rop = nullptr;
    }

    // Ltl(const Ltl &) = delete;
    // void operator=(const Ltl &) = delete;

    void build_presentation_string(std::string &s) const {
        switch (opc) {
        case TRUE:
            s.append("true");
            break;
        case FALSE:
            s.append("false");
            break;
        case ATOM:
            s.append(name);
            break;

        case IMPL:
            s.push_back('(');
            lop->build_presentation_string(s);
            s.append(" -> ");
            rop->build_presentation_string(s);
            s.push_back(')');
            break;

        case NOT:
        case G:
        case F:
        case X:
            s.push_back(symbol_of(opc));
            lop->build_presentation_string(s);
            break;

        case AND:
        case OR:
        case U:
        case R:
        case W:
            s.push_back('(');
            lop->build_presentation_string(s);
            s.push_back(' ');
            s.push_back(symbol_of(opc));
            s.push_back(' ');
            rop->build_presentation_string(s);
            s.push_back(')');
            break;
        }
    }

    Operator opc;
    std::string name, presentation;
    const Ltl *lop, *rop;
};

namespace std {
    template<>
    struct hash<Ltl> {
        std::size_t operator()(const Ltl& v) const noexcept {
            // Хешируем уже готовую строку.
            return std::hash<std::string>{}(v.get_presentation());
        }
    };

    template<>
    struct hash<const Ltl> {
        std::size_t operator()(const Ltl& v) const noexcept {
            return std::hash<Ltl>{}(v);
        }
    };

} // namespace std

class Parser {
    const char *stream;
    std::vector<const Ltl *> stack;

public:
    const Ltl * parse(const char *s) {
        stream = s;
        parse_until('\0');

        assert(stack.size() == 1);
        const Ltl *ltl = stack.back();
        stack.pop_back();

        return ltl;
    }

private:
    void parse_until(char endsym) {
        char c = *stream;
        while (c && c != endsym) {
            parse_term();
            c = *stream;
        }
        assert(c == endsym && "invalid end of stream");

        if (c && c == endsym) {
            ++stream;
        }
    }

    void skip_empty() {
        const char *s = stream;
        while (*s && isspace(*s)) {
            ++s;
        }
        stream = s;
    }

    const Ltl * parse_atom() {
        const char *end = stream;
        while (*end && islower(*end)) {
            ++end;
        }
        assert(stream < end && "invalid atom token");

        std::string name(stream, end);
        stream = end;

        if (name == "true") {
            return Ltl::True();
        }
        if (name == "false") {
            return Ltl::False();
        }
        return Ltl::atom(std::move(name));
    }

    void parse_term() {
        skip_empty();

        const char c = *stream;
        switch (c) {
        default:
            stack.push_back(parse_atom());
            break;

        case '!':
        case 'X':
        case 'F':
        case 'G':
            ++stream;
            parse1(opcode_of(c));
            break;

        case '&':
        case '|':
        case 'U':
        case 'R':
        case 'W':
            ++stream;
            parse2(opcode_of(c));
            break;

        case '-':
            assert(stream[1] == '>' && "invalid token");
            stream += 2;
            parse2(IMPL);
            break;

        case '(':
            ++stream;
            parse_until(')');
            break;

        case ')':
            break;
        }
    }

    void parse1(Operator opc) {
        parse_term();
        const Ltl *ltl = pop();
        stack.push_back(Ltl::unary(opc, *ltl));
    }

    void parse2(Operator opc) {
        parse_term();
        const Ltl *rop = pop();
        const Ltl *lop = pop();
        stack.push_back(Ltl::binary(opc, *lop, *rop));
    }

    const Ltl * pop() {
        const Ltl *ltl = stack.back();
        stack.pop_back();
        return ltl;
    }
};

class Automaton {
    using index_vec_type = std::vector<size_t>;

    std::vector<index_vec_type> adjacent;
    std::vector<index_vec_type> accepting;
    index_vec_type initial;

public:
    Automaton(const Automaton&) = delete;
    Automaton &operator=(const Automaton&) = delete;

    /// Init automaton for a given number of states
    Automaton(size_t card) {
        adjacent.resize(card);
    }

    void add_transition(size_t src, size_t dst) {
        adjacent[src].push_back(dst);
    }

    void mark_init(size_t state) {
        assert(state < adjacent.size() && "invalid state number");
        initial.push_back(state);
    }

    void mark_accept(size_t set, size_t state) {
        assert(state < adjacent.size() && "invalid state number");
        if (set >= accepting.size()) {
            accepting.resize(set + 1);
        }
        accepting[set].push_back(state);
    }

    void finalize() {
        for (index_vec_type &values : adjacent) { 
            deduplicate(values);
        }
        for (index_vec_type &values : accepting) {
            deduplicate(values);
        }
        deduplicate(initial);
    }

    void write_to(FILE *f) const {
        fprintf(f, "%zu %zu\n", adjacent.size(), accepting.size());
        write_set_to(f, initial);
        for (const index_vec_type &accepting_set : accepting) {
            write_set_to(f, accepting_set);
        }

        size_t i = 0;
        for (const index_vec_type &transitions : adjacent) {
            write_set_to(f, transitions);
            ++i;
        }
    }

    size_t card() const {
        return adjacent.size();
    }

private:
    static void write_set_to(FILE *f, const index_vec_type &values) {
        fprintf(f, "%zu ", values.size());
        for (size_t v : values) {
            fprintf(f, "%zu ", v);
        }
        fputs("\n", f);
    }

    static void deduplicate(index_vec_type &values) {
        std::sort(values.begin(), values.end());
        index_vec_type::iterator it =
            std::unique(values.begin(), values.end());
        values.erase(it, values.end());
    }
};

const Ltl * simplify_ltl(const Ltl &ltl) {
    switch (ltl.kind()) {
        case NOT: {
            const Ltl &simplified_sub_ltl = *simplify_ltl(*ltl.lhs());
            switch (simplified_sub_ltl.kind()) {
                case AND:
                    return simplify_ltl(*ltl.binary(
                        OR,
                        *ltl.unary(
                            NOT,
                            *simplified_sub_ltl.lhs()
                        ),
                        *ltl.unary(
                            NOT,
                            *simplified_sub_ltl.rhs()
                        )
                    ));
                case OR:
                    return simplify_ltl(*ltl.binary(
                        AND,
                        *ltl.unary(
                            NOT,
                            *simplified_sub_ltl.lhs()
                        ),
                        *ltl.unary(
                            NOT,
                            *simplified_sub_ltl.rhs()
                        )
                    ));
                case NOT:
                    return simplified_sub_ltl.lhs();
                case TRUE:
                    return ltl.False();
                case FALSE:
                    return ltl.True();
                default:
                    return ltl.unary(
                        NOT,
                        simplified_sub_ltl
                    );
            }
        }
        case G:
            return simplify_ltl(*ltl.unary(
                NOT,
                *ltl.unary(
                    F,
                    *ltl.unary(
                        NOT,
                        *ltl.lhs()
                    )
                )
            ));
        case F:
            return ltl.binary(
                U,
                *ltl.True(),
                *simplify_ltl(*ltl.lhs())
            );
        case X: {
            const Ltl &simplified_sub_ltl = *simplify_ltl(*ltl.lhs());
            switch (simplified_sub_ltl.kind()) {
                case TRUE:
                case FALSE:
                    return &simplified_sub_ltl;
                case NOT:
                    return simplify_ltl(*ltl.unary(
                        NOT,
                        *ltl.unary(
                            X,
                            *simplified_sub_ltl.lhs()
                        )
                    ));
                case AND:
                case OR:
                case U:
                    return simplify_ltl(*ltl.binary(
                        simplified_sub_ltl.kind(),
                        *ltl.unary(
                            X,
                            *simplified_sub_ltl.lhs()
                        ),
                        *ltl.unary(
                            X,
                            *simplified_sub_ltl.rhs()
                        )
                    ));
                default:
                    return ltl.unary(
                        X,
                        simplified_sub_ltl
                    );
            }
        }
        case AND:
        case OR:
        case U:
            return ltl.binary(
                ltl.kind(),
                *simplify_ltl(*ltl.lhs()),
                *simplify_ltl(*ltl.rhs())
            );
        case IMPL:
            return simplify_ltl(*ltl.binary(
                OR,
                *ltl.unary(NOT, *ltl.lhs()),
                *ltl.rhs()
            ));
        case R:
            return simplify_ltl(*ltl.unary(
                NOT,
                *ltl.binary(
                    U,
                    *ltl.unary(
                        NOT,
                        *ltl.lhs()
                    ),
                    *ltl.unary(
                        NOT,
                        *ltl.rhs()
                    )
                )
            ));
        case W:
            return simplify_ltl(*ltl.binary(
                OR,
                *ltl.binary(
                    U,
                    *ltl.lhs(),
                    *ltl.rhs()
                ),
                *ltl.unary(
                    G,
                    *ltl.lhs()
                )
            ));
        default:
            return &ltl;
    }
}

std::unordered_set<Ltl> get_atoms(const Ltl &simplified_ltl) {
    std::unordered_set<Ltl> atoms;
    switch (simplified_ltl.kind()) {
        case ATOM:
        case X: {
            atoms.insert(simplified_ltl);
            if (simplified_ltl.kind() == ATOM) return atoms;
        }
        case NOT:
            atoms.merge(get_atoms(*simplified_ltl.lhs()));
        case TRUE:
        case FALSE:
            return atoms;
        case OR:
        case AND:
        case U:
            atoms.merge(get_atoms(*simplified_ltl.lhs()));
            atoms.merge(get_atoms(*simplified_ltl.rhs()));
            return atoms;
        default:
            assert(false);
    }
}

std::unordered_set<Ltl>
get_positive_closure(const Ltl &simplified_ltl) {
    std::unordered_set<Ltl> positive_closure;
    switch (simplified_ltl.kind()) {
        case OR:
        case AND:
        case U:
            positive_closure.merge(get_positive_closure(*simplified_ltl.rhs()));
        case NOT:
        case X:
            positive_closure.merge(get_positive_closure(*simplified_ltl.lhs()));
        case TRUE:
        case ATOM:
            positive_closure.insert(simplified_ltl);
        case FALSE:
            return positive_closure;
        default:
            assert(false);
    }
}

template <class T>
std::vector<T> unordered_set2vector(const std::unordered_set<T> &src) {
    return std::vector<T>(src.begin(), src.end());
}

std::vector<Ltl>
get_sorted_positive_closure(const Ltl &simplified_ltl) {
    std::vector<Ltl> positive_closures = unordered_set2vector(get_positive_closure(simplified_ltl));
    std::sort(  // From easiest to hardest
        positive_closures.begin(),
        positive_closures.end(),
        [](const Ltl&a, const Ltl &b)
        {
            return a.get_presentation().size() < b.get_presentation().size();
        }
    );
    return positive_closures;
}

std::vector<std::vector<std::unordered_set<Ltl>>> saturate(
    const std::vector<Ltl> &atoms,
    const std::vector<Ltl> &sorted_positive_closure
) {
    std::vector<std::vector<std::unordered_set<Ltl>>> saturations;
    size_t combinations_num = 1u << atoms.size();
    for (size_t mask = 0; mask < combinations_num; mask++) {
        std::unordered_set<Ltl> classic_saturations;
        for (size_t atom_ind = 0; atom_ind < atoms.size(); atom_ind++) {
            if (mask & (1u << atom_ind)) {
                classic_saturations.insert(atoms[atom_ind]);
            } else {
                classic_saturations.insert(*Ltl::unary(NOT, atoms[atom_ind]));
            }
        }

        std::vector<Ltl> subformulas;
        for (auto positive_closure : sorted_positive_closure) {
            bool found_in_saturated = false;
            for (auto classic_saturation : classic_saturations) {
                if (classic_saturation == positive_closure) {
                    found_in_saturated = true;
                    break;
                }
            }
            if (!found_in_saturated) subformulas.push_back(positive_closure);
        }

        std::vector<std::unordered_set<Ltl>> temporal_saturations;
        temporal_saturations.push_back(classic_saturations);
        for (auto subformula : subformulas) {
            std::vector<std::unordered_set<Ltl>> new_temporal_saturations;
            for (std::unordered_set<Ltl> &temporal_saturation : temporal_saturations) {
                switch (subformula.kind()) {
                    case TRUE:
                        temporal_saturation.insert(subformula);
                    case FALSE:
                    case ATOM:
                    case X:
                        break;
                    case NOT: {
                        const Ltl *subformula_lhs = subformula.lhs();
                        assert(subformula_lhs->kind() == U || subformula_lhs->kind() == X || subformula_lhs->kind() == ATOM);
                        if (!temporal_saturation.count(*subformula.lhs())) {
                            temporal_saturation.insert(subformula);
                        }
                        break;
                    }
                    case OR:
                        if (
                            temporal_saturation.count(*subformula.lhs())
                            || temporal_saturation.count(*subformula.rhs())
                        ) {
                            temporal_saturation.insert(subformula);
                        }
                        break;
                    case AND:
                        if (
                            temporal_saturation.count(*subformula.lhs())
                            && temporal_saturation.count(*subformula.rhs())
                        ) {
                            temporal_saturation.insert(subformula);
                        }
                        break;
                    case U:
                        if (temporal_saturation.count(*subformula.rhs())) {
                            temporal_saturation.insert(subformula);
                        } else if (temporal_saturation.count(*subformula.lhs())) {
                            std::unordered_set<Ltl> new_temporal_saturation = temporal_saturation;
                            new_temporal_saturation.insert(subformula);
                            new_temporal_saturations.push_back(new_temporal_saturation);
                        }
                        break;
                    default:
                        assert(false);
                }
            }
            for (auto new_temporal_saturation : new_temporal_saturations) {
                temporal_saturations.push_back(new_temporal_saturation);
            }
        }
        saturations.push_back(temporal_saturations);
    }
    return saturations;
}

std::vector<std::unordered_set<Ltl>>
get_states(
    const std::vector<Ltl> &atoms,
    const std::vector<Ltl> &sorted_positive_closure
) {
    const std::vector<std::vector<std::unordered_set<Ltl>>> &saturation = saturate(atoms, sorted_positive_closure);
    std::vector<std::unordered_set<Ltl>> states;
    for (auto atom_comb : saturation) {
        for (auto state : atom_comb) {
            states.push_back(state);
        }
    }
    return states;
}

std::vector<std::vector<size_t>>
make_transitions(
    const std::vector<std::unordered_set<Ltl>> &states,
    const std::vector<Ltl> &positive_closures
) {
    std::vector<std::vector<size_t>> transitions;
    for (size_t state1_ind = 0; state1_ind < states.size(); state1_ind++) {
        const std::unordered_set<Ltl> &state1 = states[state1_ind];
        std::vector<size_t> transitions_from_state1;
        for (size_t state2_ind = 0; state2_ind < states.size(); state2_ind++) {
            const std::unordered_set<Ltl> &state2 = states[state2_ind];
            bool add_transition = true;
            for (auto positive_closure : positive_closures) {
                bool left_cond = true, right_cond = true;
                if (positive_closure.kind() == X) {
                    left_cond = state1.count(positive_closure);
                    right_cond = state2.count(*positive_closure.lhs());
                } else if (positive_closure.kind() == U) {
                    left_cond = state1.count(positive_closure);
                    right_cond =
                        state1.count(*positive_closure.rhs())
                        || (state1.count(*positive_closure.lhs()) && state2.count(positive_closure));
                    
                }
                if (left_cond != right_cond) {
                    add_transition = false;
                    break;
                }
            }
            if (add_transition) {
                transitions_from_state1.push_back(state2_ind);
            }
        }
        transitions.push_back(transitions_from_state1);
    }
    return transitions;
}

std::vector<size_t>
get_start_states(
    const std::vector<std::unordered_set<Ltl>> &states,
    const Ltl &final_formula
) {
    std::vector<size_t> start_states;
    for (size_t state_ind = 0; state_ind < states.size(); state_ind++) {
        if (states[state_ind].count(final_formula)) start_states.push_back(state_ind);
    }
    return start_states;
}

std::vector<std::vector<size_t>>
get_allowing_states(
    const std::vector<std::unordered_set<Ltl>> &states,
    const std::vector<Ltl> &positive_closures
) {
    std::vector<std::vector<size_t>> allowing_states;
    for (auto positive_closure : positive_closures) {
        if (positive_closure.kind() != U) continue;
        std::vector<size_t> allowing_state;
        for (size_t state_ind = 0; state_ind < states.size(); state_ind++) {
            const std::unordered_set<Ltl> &state = states[state_ind];
            if (
                !state.count(positive_closure)
                || state.count(*positive_closure.rhs())
            ) {
                allowing_state.push_back(state_ind);
            }
        }
        allowing_states.push_back(allowing_state);
    }
    return allowing_states;
}

std::unique_ptr<Automaton>
build_automaton(
    const std::vector<std::vector<size_t>> &transitions,
    const std::vector<size_t> &start_states,
    const std::vector<std::vector<size_t>> &allowing_states
) {
    std::unique_ptr<Automaton> automaton = std::make_unique<Automaton>(transitions.size());

    for (auto start_state : start_states) automaton->mark_init(start_state);

    for (size_t allowing_state_ind = 0; allowing_state_ind < allowing_states.size(); allowing_state_ind++)
    for (auto state_in_allow : allowing_states[allowing_state_ind]) automaton->mark_accept(allowing_state_ind, state_in_allow);

    for (size_t from_ind = 0; from_ind < transitions.size(); from_ind++)
    for (auto to : transitions[from_ind]) automaton->add_transition(from_ind, to);

    return automaton;
}



std::unique_ptr<Automaton>
run_ltl_to_buchi(const char *text) {
    Parser parser;
    const Ltl *ltl = parser.parse(text);

    const Ltl &simplified_ltl = *simplify_ltl(*ltl);
    const std::vector<Ltl> &atoms = unordered_set2vector(get_atoms(simplified_ltl));
    const std::vector<Ltl> &sorted_positive_closure = get_sorted_positive_closure(simplified_ltl);
    const std::vector<std::unordered_set<Ltl>> &states = get_states(atoms, sorted_positive_closure);
    const std::vector<std::vector<size_t>> &transitions = make_transitions(states, sorted_positive_closure);
    const std::vector<size_t, std::allocator<size_t>> &start_states = get_start_states(states, simplified_ltl);
    const std::vector<std::vector<size_t>> &allowing_states = get_allowing_states(states, sorted_positive_closure);

    return build_automaton(transitions, start_states, allowing_states);
}

int
main(int argc, char *argv[]) {
    std::unique_ptr<Automaton> buchi;
    for (int i = 1; i < argc; ++i) {
        buchi = run_ltl_to_buchi(argv[i]);
        if (buchi) {
            buchi->write_to(stdout);
        }
    }
    return 0;
}
