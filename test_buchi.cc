#include "ref_ptr.h"
#include <functional>
#include <cassert>
#include <cstddef>

#include <algorithm>
#include <numeric>
#include <memory>
#include <string>
#include <vector>
#include <set>
#include <map>

#include <iostream>

enum Operator
{
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

static const struct
{
    char sym, opc;
} opcodes[] = {
    {'!', NOT},
    {'&', AND},
    {'|', OR},
    {'U', U},
    {'F', F},
    {'G', G},
    {'R', R},
    {'W', W},
    {'X', X}};

static char
symbol_of(Operator opc)
{
    for (const auto &item : opcodes)
    {
        if (item.opc == opc)
        {
            return item.sym;
        }
    }
    return '\0';
}

static Operator
opcode_of(char c)
{
    for (const auto &item : opcodes)
    {
        if (item.sym == c)
        {
            return static_cast<Operator>(item.opc);
        }
    }
    return FALSE;
}

class Ltl
{
    friend void ref_ptr_inc_ref(Ltl &);
    friend void ref_ptr_release(Ltl &);

public:
    using ref_type = ref_ptr<Ltl>;

    static ref_type True()
    {
        static const ref_type ltl_true = new Ltl(TRUE);
        return ltl_true;
    }

    static ref_type False()
    {
        static const ref_type ltl_false = new Ltl(FALSE);
        return ltl_false;
    }

    static ref_type atom(std::string name)
    {
        return new Ltl(std::move(name));
    }

    static ref_type unary(Operator opc, const ref_type &opnd)
    {
        ref_type ltl = new Ltl(opc);
        ltl->lop = opnd;

        return ltl;
    }

    static ref_type binary(Operator opc, const ref_type &lop, const ref_type &rop)
    {
        ref_type ltl = new Ltl(opc);
        ltl->lop = lop;
        ltl->rop = rop;

        return ltl;
    }

    Operator kind() const
    {
        return opc;
    }

    Ltl *lhs() const
    {
        return lop.get();
    }

    Ltl *rhs() const
    {
        return rop.get();
    }

    void to_string(std::string &s) const
    {
        switch (opc)
        {
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
            lop->to_string(s);
            s.append(" -> ");
            rop->to_string(s);
            s.push_back(')');
            break;

        case NOT:
        case G:
        case F:
        case X:
            s.push_back(symbol_of(opc));
            lop->to_string(s);
            break;

        case AND:
        case OR:
        case U:
        case R:
        case W:
            s.push_back('(');
            lop->to_string(s);
            s.push_back(' ');
            s.push_back(symbol_of(opc));
            s.push_back(' ');
            rop->to_string(s);
            s.push_back(')');
            break;
        }
    }

    Ltl::ref_type process_negation(Ltl::ref_type expr) const
    {
        switch (expr->kind())
        {
        case NOT:
            return expr->lhs();
        case TRUE:
            return Ltl::False();
        case FALSE:
            return Ltl::True();
        default:
            return Ltl::unary(NOT, expr);
        }
    }

    Ltl::ref_type create_negation(Ltl::ref_type expr) const
    {
        return Ltl::unary(NOT, expr);
    }

    Ltl::ref_type handle_temporal(Ltl::ref_type expr) const
    {
        if (expr->kind() == NOT)
        {
            return Ltl::unary(NOT,
                              Ltl::unary(X, expr->lhs()));
        }
        if (expr->kind() == OR || expr->kind() == U)
        {
            return Ltl::binary(expr->kind(),
                               Ltl::unary(X, expr->lhs()),
                               Ltl::unary(X, expr->rhs()));
        }
        return Ltl::unary(X, expr);
    }

    Ltl::ref_type normalize() const
    {
        Ltl::ref_type left, right;
        switch (opc)
        {
        case TRUE:
        case FALSE:
        case ATOM:
            return const_cast<Ltl *>(this);

        case NOT:
            return process_negation(lop->normalize());

        case G:
            return create_negation(
                Ltl::binary(U, Ltl::True(),
                            Ltl::unary(NOT, lop->normalize())));

        case F:
            return Ltl::binary(U, Ltl::True(), lop->normalize());

        case X:
            return handle_temporal(lop->normalize());

        case IMPL:
            return Ltl::binary(OR,
                               Ltl::unary(NOT, lop->normalize()),
                               rop->normalize());

        case AND:
            return create_negation(
                Ltl::binary(OR,
                            Ltl::unary(NOT, lop->normalize()),
                            Ltl::unary(NOT, rop->normalize())));

        case OR:
        case U:
            return Ltl::binary(opc, lop->normalize(), rop->normalize());

        case R:
            return create_negation(
                Ltl::binary(U,
                            Ltl::unary(NOT, lop->normalize()),
                            Ltl::unary(NOT, rop->normalize())));

        case W:
            return Ltl::binary(OR,
                               Ltl::binary(U, lop->normalize(), rop->normalize()),
                               create_negation(
                                   Ltl::binary(U, Ltl::True(),
                                               Ltl::unary(NOT, lop->normalize()))));
        }
        return nullptr;
    }

    std::set<std::string> collect_temporal_components() const
    {
        std::set<std::string> components;
        std::string repr;

        switch (opc)
        {
        case TRUE:
        case FALSE:
            break;

        case ATOM:
            to_string(repr);
            components.insert(repr);
            break;

        case X:
            to_string(repr);
            components.insert(repr);
            for (const auto &s : lop->collect_temporal_components())
            {
                components.insert(s);
            }
            break;

        case NOT:
            return lop->collect_temporal_components();

        case OR:
        case U:
            for (const auto &s : lop->collect_temporal_components())
            {
                components.insert(s);
            }
            for (const auto &s : rop->collect_temporal_components())
            {
                components.insert(s);
            }
            break;

        default:
            break;
        }
        return components;
    }

    std::vector<std::set<std::string>> compute_labelings(
        const std::map<std::string, bool> &assignments) const
    {
        std::vector<std::set<std::string>> result;
        std::string current_str;
        this->to_string(current_str);

        switch (opc)
        {
        case TRUE:
        {
            result.push_back({current_str});
            break;
        }

        case FALSE:
        {
            result.push_back({});
            break;
        }

        case ATOM:
        {
            if (assignments.at(current_str))
            {
                result.push_back({current_str});
            }
            else
            {
                result.push_back({});
            }
            break;
        }

        case NOT:
        {
            auto child_labels = lop->compute_labelings(assignments);
            std::string child_str;
            lop->to_string(child_str);

            for (auto &labels : child_labels)
            {
                if (labels.count(child_str) == 0)
                {
                    labels.insert(current_str);
                }
            }
            return child_labels;
        }

        case OR:
        {
            auto left_labels = lop->compute_labelings(assignments);
            auto right_labels = rop->compute_labelings(assignments);
            std::string left_str, right_str;
            lop->to_string(left_str);
            rop->to_string(right_str);

            for (const auto &l : left_labels)
            {
                for (const auto &r : right_labels)
                {
                    std::set<std::string> combined;
                    combined.insert(l.begin(), l.end());
                    combined.insert(r.begin(), r.end());

                    if (combined.count(left_str) || combined.count(right_str))
                    {
                        combined.insert(current_str);
                    }
                    result.push_back(combined);
                }
            }
            break;
        }

        case AND:
        {
            auto not_left = Ltl::unary(NOT, lop);
            auto not_right = Ltl::unary(NOT, rop);
            auto or_expr = Ltl::binary(OR, not_left, not_right);
            auto not_expr = Ltl::unary(NOT, or_expr);
            return not_expr->compute_labelings(assignments);
        }

        case X:
        {
            auto child_labels = lop->compute_labelings(assignments);
            if (assignments.at(current_str))
            {
                for (auto &labels : child_labels)
                {
                    labels.insert(current_str);
                }
            }
            return child_labels;
        }

        case U:
        {
            auto left_labels = lop->compute_labelings(assignments);
            auto right_labels = rop->compute_labelings(assignments);
            std::string left_str, right_str;
            lop->to_string(left_str);
            rop->to_string(right_str);

            for (const auto &l : left_labels)
            {
                for (const auto &r : right_labels)
                {
                    std::set<std::string> combined;
                    combined.insert(l.begin(), l.end());
                    combined.insert(r.begin(), r.end());

                    if (combined.count(right_str))
                    {
                        combined.insert(current_str);
                        result.push_back(combined);
                    }
                    else
                    {
                        result.push_back(combined);

                        if (combined.count(left_str))
                        {
                            std::set<std::string> with_u(combined);
                            with_u.insert(current_str);
                            result.push_back(with_u);
                        }
                    }
                }
            }
            break;
        }

        case F:
        {
            auto u_expr = Ltl::binary(U, Ltl::True(), lop);
            return u_expr->compute_labelings(assignments);
        }

        case G:
        {
            auto not_phi = Ltl::unary(NOT, lop);
            auto u_expr = Ltl::binary(U, Ltl::True(), not_phi);
            auto not_expr = Ltl::unary(NOT, u_expr);
            return not_expr->compute_labelings(assignments);
        }

        case IMPL:
        {
            auto not_left = Ltl::unary(NOT, lop);
            auto or_expr = Ltl::binary(OR, not_left, rop);
            return or_expr->compute_labelings(assignments);
        }

        case R:
        {
            auto not_phi = Ltl::unary(NOT, lop);
            auto not_psi = Ltl::unary(NOT, rop);
            auto u_expr = Ltl::binary(U, not_phi, not_psi);
            auto not_expr = Ltl::unary(NOT, u_expr);
            return not_expr->compute_labelings(assignments);
        }

        case W:
        {
            auto u_part = Ltl::binary(U, lop, rop);
            auto not_phi = Ltl::unary(NOT, lop);
            auto g_part = Ltl::binary(U, Ltl::True(), not_phi);
            auto or_expr = Ltl::binary(OR, u_part, Ltl::unary(NOT, g_part));
            return or_expr->compute_labelings(assignments);
        }

        default:
            result.push_back({});
        }

        return result;
    }

    void filter_invalid_transitions(const std::vector<std::set<std::string>> &state_labels,
                                    std::vector<std::set<size_t>> &transition_matrix) const
    {
        switch (opc)
        {
        case TRUE:
        case FALSE:
        case ATOM:
            return;

        case NOT:
            lop->filter_invalid_transitions(state_labels, transition_matrix);
            return;

        case OR:
            lop->filter_invalid_transitions(state_labels, transition_matrix);
            rop->filter_invalid_transitions(state_labels, transition_matrix);
            return;

        case X:
            process_next_operator(state_labels, transition_matrix);
            return;

        case U:
            process_until_operator(state_labels, transition_matrix);
            return;

        default:
            if (lop)
                lop->filter_invalid_transitions(state_labels, transition_matrix);
            if (rop)
                rop->filter_invalid_transitions(state_labels, transition_matrix);
        }
    }

    std::vector<std::set<size_t>> compute_acceptance_sets(
        const std::vector<std::set<std::string>> &state_labels) const
    {
        std::vector<std::set<size_t>> acceptance_sets;

        switch (opc)
        {
        case TRUE:
        case FALSE:
        case ATOM:
            return {};

        case NOT:
        case X:
            return lhs()->compute_acceptance_sets(state_labels);

        case OR:
            return combine_acceptance_sets(
                lhs()->compute_acceptance_sets(state_labels),
                rhs()->compute_acceptance_sets(state_labels));

        case U:
            return process_until_acceptance(state_labels);

        default:
            if (lhs() && rhs())
            {
                return combine_acceptance_sets(
                    lhs()->compute_acceptance_sets(state_labels),
                    rhs()->compute_acceptance_sets(state_labels));
            }
            return {};
        }
    }

private:
    void process_next_operator(const std::vector<std::set<std::string>> &labels,
                               std::vector<std::set<size_t>> &transitions) const
    {
        std::string current_op, child_op;
        to_string(current_op);
        lop->to_string(child_op);

        for (size_t i = 0; i < labels.size(); ++i)
        {
            for (size_t j = 0; j < labels.size(); ++j)
            {
                bool has_current = labels[i].count(current_op);
                bool has_child = labels[j].count(child_op);

                if ((has_current && !has_child) || (!has_current && has_child))
                {
                    transitions[i].erase(j);
                }
            }
        }
        lop->filter_invalid_transitions(labels, transitions);
    }

    void process_until_operator(const std::vector<std::set<std::string>> &labels,
                                std::vector<std::set<size_t>> &transitions) const
    {
        std::string until_op, left_op, right_op;
        to_string(until_op);
        lop->to_string(left_op);
        rop->to_string(right_op);

        for (size_t i = 0; i < labels.size(); ++i)
        {
            for (size_t j = 0; j < labels.size(); ++j)
            {
                bool has_until = labels[i].count(until_op);
                bool has_right = labels[i].count(right_op);
                bool has_left = labels[i].count(left_op);
                bool has_next_until = labels[j].count(until_op);

                bool valid = (has_until && (has_right || (has_left && has_next_until))) ||
                             (!has_until && !(has_right || (has_left && has_next_until)));

                if (!valid)
                {
                    transitions[i].erase(j);
                }
            }
        }

        lop->filter_invalid_transitions(labels, transitions);
        rop->filter_invalid_transitions(labels, transitions);
    }

    std::vector<std::set<size_t>> combine_acceptance_sets(
        std::vector<std::set<size_t>> left,
        std::vector<std::set<size_t>> right) const
    {
        left.insert(left.end(), right.begin(), right.end());
        return left;
    }

    std::vector<std::set<size_t>> process_until_acceptance(
        const std::vector<std::set<std::string>> &labels) const
    {
        auto left_sets = lhs()->compute_acceptance_sets(labels);
        auto right_sets = rhs()->compute_acceptance_sets(labels);

        std::string until_op, right_op;
        to_string(until_op);
        rop->to_string(right_op);

        std::set<size_t> until_states;
        for (size_t i = 0; i < labels.size(); ++i)
        {
            if (!labels[i].count(until_op) || labels[i].count(right_op))
            {
                until_states.insert(i);
            }
        }

        right_sets.push_back(until_states);
        return combine_acceptance_sets(left_sets, right_sets);
    }

    Ltl(std::string _name)
    {
        nref = 0;
        opc = ATOM;
        name = std::move(_name);
        lop = nullptr;
        rop = nullptr;
    }

    Ltl(Operator _opc)
    {
        nref = 0;
        opc = _opc;
        lop = nullptr;
        rop = nullptr;
    }

    Ltl(const Ltl &) = delete;
    void operator=(const Ltl &) = delete;

    int nref;
    Operator opc;
    std::string name;
    ref_type lop, rop;
};

void ref_ptr_inc_ref(Ltl &x)
{
    ++x.nref;
}

void ref_ptr_release(Ltl &x)
{
    --x.nref;
    if (x.nref <= 0)
    {
        delete &x;
    }
}

class Parser
{
    const char *stream;
    std::vector<ref_ptr<Ltl>> stack;

public:
    ref_ptr<Ltl> parse(const char *s)
    {
        stream = s;
        parse_until('\0');

        assert(stack.size() == 1);
        ref_ptr<Ltl> ltl = stack.back();
        stack.pop_back();

        return ltl;
    }

private:
    void parse_until(char endsym)
    {
        char c = *stream;
        while (c && c != endsym)
        {
            parse_term();
            c = *stream;
        }
        assert(c == endsym && "invalid end of stream");

        if (c && c == endsym)
        {
            ++stream;
        }
    }

    void skip_empty()
    {
        const char *s = stream;
        while (*s && isspace(*s))
        {
            ++s;
        }
        stream = s;
    }

    ref_ptr<Ltl> parse_atom()
    {
        const char *end = stream;
        while (*end && islower(*end))
        {
            ++end;
        }
        assert(stream < end && "invalid atom token");

        std::string name(stream, end);
        stream = end;

        if (name == "true")
        {
            return Ltl::True();
        }
        if (name == "false")
        {
            return Ltl::False();
        }
        return Ltl::atom(std::move(name));
    }

    void parse_term()
    {
        skip_empty();

        const char c = *stream;
        switch (c)
        {
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

    void parse1(Operator opc)
    {
        parse_term();
        ref_ptr<Ltl> ltl = pop();
        stack.push_back(Ltl::unary(opc, ltl));
    }

    void parse2(Operator opc)
    {
        parse_term();
        ref_ptr<Ltl> rop = pop();
        ref_ptr<Ltl> lop = pop();
        stack.push_back(Ltl::binary(opc, lop, rop));
    }

    ref_ptr<Ltl> pop()
    {
        ref_ptr<Ltl> ltl = stack.back();
        stack.pop_back();
        return ltl;
    }
};

class Automaton
{
    using index_vec_type = std::vector<size_t>;

    std::vector<index_vec_type> adjacent;
    std::vector<index_vec_type> accepting;
    index_vec_type initial;

public:
    Automaton(const Automaton &) = delete;
    Automaton &operator=(const Automaton &) = delete;

    /// Init automaton for a given number of states
    Automaton(size_t card)
    {
        adjacent.resize(card);
    }

    void add_transition(size_t src, size_t dst)
    {
        adjacent[src].push_back(dst);
    }

    void mark_init(size_t state)
    {
        assert(state < adjacent.size() && "invalid state number");
        initial.push_back(state);
    }

    void mark_accept(size_t set, size_t state)
    {
        assert(state < adjacent.size() && "invalid state number");
        if (set >= accepting.size())
        {
            accepting.resize(set + 1);
        }
        accepting[set].push_back(state);
    }

    void finalize()
    {
        for (index_vec_type &values : adjacent)
        {
            deduplicate(values);
        }
        for (index_vec_type &values : accepting)
        {
            deduplicate(values);
        }
        deduplicate(initial);
    }

    void write_to(FILE *f) const
    {
        fprintf(f, "%zu %zu\n", adjacent.size(), accepting.size());
        write_set_to(f, initial);
        for (const index_vec_type &accepting_set : accepting)
        {
            write_set_to(f, accepting_set);
        }

        size_t i = 0;
        for (const index_vec_type &transitions : adjacent)
        {
            write_set_to(f, transitions);
            ++i;
        }
    }

    size_t card() const
    {
        return adjacent.size();
    }

private:
    static void write_set_to(FILE *f, const index_vec_type &values)
    {
        fprintf(f, "%zu ", values.size());
        for (size_t v : values)
        {
            fprintf(f, "%zu ", v);
        }
        fputs("\n", f);
    }

    static void deduplicate(index_vec_type &values)
    {
        std::sort(values.begin(), values.end());
        index_vec_type::iterator it =
            std::unique(values.begin(), values.end());
        values.erase(it, values.end());
    }
};

void generate_assignments_recursively(
    const Ltl::ref_type &formula,
    const std::vector<std::string> &elements,
    std::vector<bool> &current_assignment,
    std::vector<std::set<std::string>> &result)
{
    if (current_assignment.size() == elements.size())
    {
        std::map<std::string, bool> assignment_map;
        for (size_t i = 0; i < elements.size(); ++i)
        {
            assignment_map[elements[i]] = current_assignment[i];
        }
        auto labelings = formula->compute_labelings(assignment_map);
        result.insert(result.end(), labelings.begin(), labelings.end());
        return;
    }
    current_assignment.push_back(false);
    generate_assignments_recursively(formula, elements, current_assignment, result);
    current_assignment.pop_back();

    current_assignment.push_back(true);
    generate_assignments_recursively(formula, elements, current_assignment, result);
    current_assignment.pop_back();
}

std::vector<std::set<std::string>> generate_all_labelings(
    const Ltl::ref_type &formula,
    const std::vector<std::string> &elements)
{
    std::vector<std::set<std::string>> result;
    std::vector<bool> current_assignment;
    current_assignment.reserve(elements.size());

    generate_assignments_recursively(formula, elements, current_assignment, result);
    return result;
}

static std::unique_ptr<Automaton>
run_ltl_to_buchi(const char *text)
{
    Parser parser;
    Ltl::ref_type formula = parser.parse(text);
    Ltl::ref_type simplified = formula->normalize();
    std::string formula_str;
    simplified->to_string(formula_str);

    auto elementary = simplified->collect_temporal_components();
    std::vector<std::string> elem_list(elementary.begin(), elementary.end());


    auto state_labels = generate_all_labelings(simplified, elem_list);

    auto automaton = std::make_unique<Automaton>(state_labels.size());

    for (size_t i = 0; i < state_labels.size(); i++)
    {
        if (state_labels[i].count(formula_str))
        {
            automaton->mark_init(i);
        }
    }

    std::vector<std::set<size_t>> transitions(state_labels.size());
    for (size_t i = 0; i < state_labels.size(); i++)
    {
        for (size_t j = 0; j < state_labels.size(); j++)
        {
            transitions[i].insert(j);
        }
    }
    simplified->filter_invalid_transitions(state_labels, transitions);

    for (size_t i = 0; i < transitions.size(); i++)
    {
        for (size_t j : transitions[i])
        {
            automaton->add_transition(i, j);
        }
    }

    auto acceptance_sets = simplified->compute_acceptance_sets(state_labels);
    for (size_t i = 0; i < acceptance_sets.size(); i++)
    {
        for (size_t state : acceptance_sets[i])
        {
            automaton->mark_accept(i, state);
        }
    }

    return automaton;
}

int main(int argc, char *argv[])
{
    std::unique_ptr<Automaton> buchi;
    for (int i = 1; i < argc; ++i)
    {
        buchi = run_ltl_to_buchi(argv[i]);
        if (buchi)
        {
            buchi->write_to(stdout);
        }
    }
    return 0;
}
