#include "ref_ptr.h"

#include <cassert>
#include <cstddef>

#include <set>
#include <algorithm>
#include <memory>
#include <string>
#include <vector>

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
    friend void ref_ptr_inc_ref(Ltl &);
    friend void ref_ptr_release(Ltl &);

public:
    using ref_type = ref_ptr<Ltl>;

    static ref_type True() {
        static const ref_type ltl_true = new Ltl(TRUE);
        return ltl_true;
    }

    static ref_type False() {
        static const ref_type ltl_false = new Ltl(FALSE);
        return ltl_false;
    }

    static ref_type atom(std::string name) {
        return new Ltl(std::move(name));
    }

    static ref_type unary(Operator opc, const ref_type &opnd) {
        ref_type ltl = new Ltl(opc);
        ltl->lop = opnd;

        return ltl;
    }

    static ref_type binary(Operator opc, const ref_type &lop, const ref_type &rop) {
        ref_type ltl = new Ltl(opc);
        ltl->lop = lop;
        ltl->rop = rop;

        return ltl;
    }

    Operator kind() const {
        return opc;
    }

    const Ltl *lhs() const {
        return lop.get();
    }

    const Ltl *rhs() const {
        return rop.get();
    }

    ref_type get_lop() {
        return lop;
    }
    
    void set_lop(ref_type nlop) {
        lop = nlop;
    }

    ref_type get_rop() {
        return rop;
    }

    void set_rop(ref_type nrop) {
        rop=nrop;
    }

    void to_string(std::string &s) const {
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
    
private:
    Ltl(std::string _name) {
        nref = 0;
        opc = ATOM;
        name = std::move(_name);
        lop = nullptr;
        rop = nullptr;
    }

    Ltl(Operator _opc) {
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

void
ref_ptr_inc_ref(Ltl &x) {
    ++x.nref;
}

void
ref_ptr_release(Ltl &x) {
    --x.nref;
    if (x.nref <= 0) {
        delete &x;
    }
}

class Parser {
    const char *stream;
    std::vector<ref_ptr<Ltl>> stack;

public:
    ref_ptr<Ltl> parse(const char *s) {
        stream = s;
        parse_until('\0');

        assert(stack.size() == 1);
        ref_ptr<Ltl> ltl = stack.back();
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

    ref_ptr<Ltl> parse_atom() {
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
            ++stream;
            parse1(opcode_of(c));
            break;
        case 'F':       // Fp => true U p
            {
                ++stream;
                parse_term();
                ref_ptr<Ltl> ltl = pop();
                stack.push_back(Ltl::binary(U, Ltl::True(), ltl));
                break;
            }
        case 'G':       // Gp => !(true U !p)
            {
                ++stream;
                parse_term();
                ref_ptr<Ltl> ltl = pop();
                ltl = Ltl::unary(NOT, ltl);
                ltl = Ltl::binary(U, Ltl::True(), ltl);
                ltl = Ltl::unary(NOT, ltl);
                stack.push_back(ltl);
                break;
            }
        case '&':       // p & q => !(!p | !q)
            {
                ++stream;
                parse_term();
                ref_ptr<Ltl> rop = pop();
                rop = Ltl::unary(NOT, rop);
                ref_ptr<Ltl> lop = pop();
                lop = Ltl::unary(NOT, lop);
                stack.push_back(Ltl::unary(NOT, Ltl::binary(OR, lop, rop)));
                break;
            }
        case '|':
        case 'U':
            ++stream;
            parse2(opcode_of(c));
            break;
        case 'R':       // p R q => !(!p U !q)
            {
                ++stream;
                parse_term();
                ref_ptr<Ltl> rop = pop();
                ref_ptr<Ltl> lop = pop();
                lop = Ltl::unary(NOT, lop);
                rop = Ltl::unary(NOT, rop);
                lop = Ltl::binary(U, lop, rop);
                stack.push_back(Ltl::unary(NOT, lop));
                break;
            }
        case 'W':       // p W q => (p U q) | !(true U !p)
            {
                ++stream;
                parse_term();
                ref_ptr<Ltl> rop = pop();
                ref_ptr<Ltl> lop = pop();
                ref_ptr<Ltl> ltl1 = lop;
                ltl1 = Ltl::unary(NOT, ltl1);
                ltl1 = Ltl::binary(U, Ltl::True(), ltl1);
                ltl1 = Ltl::unary(NOT, ltl1);
                ref_ptr<Ltl> ltl2 = Ltl::binary(U, lop, rop);
                stack.push_back(Ltl::binary(OR, ltl2, ltl1));
                break;
            }
           
        case '-':       // p -> q => !p | q
            {
                assert(stream[1] == '>' && "invalid token");
                stream += 2;
                parse_term();
                ref_ptr<Ltl> rop = pop();
                ref_ptr<Ltl> lop = pop();
                lop = Ltl::unary(NOT, lop);
                stack.push_back(Ltl::binary(OR, lop, rop));
                break;
            }
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
        ref_ptr<Ltl> ltl = pop();
        stack.push_back(Ltl::unary(opc, ltl));
    }

    void parse2(Operator opc) {
        parse_term();
        ref_ptr<Ltl> rop = pop();
        ref_ptr<Ltl> lop = pop();
        stack.push_back(Ltl::binary(opc, lop, rop));
    }

    ref_ptr<Ltl> pop() {
        ref_ptr<Ltl> ltl = stack.back();
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

static std::unique_ptr<Automaton>
run_ltl_to_buchi(const char *text) {
    Parser parser;
    ref_ptr<Ltl> ltl = parser.parse(text);

    std::string f;
    ltl->to_string(f);
    fputs(f.c_str(), stderr);
    return nullptr;
}

void init(ref_ptr<Ltl> &ltl, std::set<std::string> &atoms, int x = 0) {
    switch (ltl->kind())
    {
    case NOT:
        {
            ref_ptr<Ltl> next_ltl = ltl->get_lop();
            if (next_ltl->kind() != NOT) {
                init(next_ltl, atoms, x);
                ltl->set_lop(next_ltl);
            } else {
                ltl = next_ltl->get_lop();
                init(ltl, atoms, x);
            }
            break;
        }
    case OR:
    case U:
        {
            ref_ptr<Ltl> next_lop = ltl->get_lop();
            ref_ptr<Ltl> next_rop = ltl->get_rop();
            init(next_lop, atoms, x);
            ltl->set_lop(next_lop);
            init(next_rop, atoms, x);
            ltl->set_rop(next_rop);
            break;
        }
    case X:
        {
            ltl = ltl->get_lop();
            init(ltl, atoms, x + 1);
            break;
        }
    case TRUE:
        break;
    case FALSE:
        break;
    default:
        {
            for (int i = 0; i <= x; i++) {
                if (i) {
                    ltl = Ltl::unary(X, ltl);
                }
                std::string f;
                ltl->to_string(f);
                atoms.insert(f);
            }
            break;
        }
    }
}

void closure(std::vector <std::set <std::string>> &states, ref_ptr<Ltl> &ltl, int start) {
    std::string ins;
    ltl->to_string(ins);
    switch (ltl->kind())
    {
    case NOT:
        {
            ref_ptr<Ltl> next = ltl->get_lop();
            std::string f;
            next->to_string(f);
            closure(states, next, start);
            for (int i = start; i < states.size(); i++) {
                if (states[i].find(f) == states[i].end()) {
                    states[i].insert(ins);
                }
            }
            break;
        }
    case OR:
        {
            ref_ptr<Ltl> next_lop = ltl->get_lop();
            ref_ptr<Ltl> next_rop = ltl->get_rop();
            std::string fl, fr;
            next_lop->to_string(fl);
            next_rop->to_string(fr);
            closure(states, next_lop, start);
            closure(states, next_rop, start);
            for (int i = start; i < states.size(); i++) {
                if (states[i].find(fl) != states[i].end() || states[i].find(fr) != states[i].end()) {
                    states[i].insert(ins);
                }
            }
            break;
        }
    case U:
        {
            ref_ptr<Ltl> next_lop = ltl->get_lop();
            ref_ptr<Ltl> next_rop = ltl->get_rop();
            std::string fl, fr;
            next_lop->to_string(fl);
            next_rop->to_string(fr);
            closure(states, next_lop, start);
            closure(states, next_rop, start);
            for (int i = start; i < states.size(); i++) {
                if (states[i].find(fr) != states[i].end()) {
                    states[i].insert(ins);
                } else if (states[i].find(fl) != states[i].end()) {
                    states.push_back(states[i++]);
                    states[i].insert(ins);
                }
            }
            break;
        }
    }
}

void connect(std::vector <std::set <std::string>> &states, ref_ptr<Ltl> &ltl, int &cur_state, std::set <int> &adj) {
    std::string ins;
    ltl->to_string(ins);
    switch (ltl->kind())
    {
    case NOT:
        {
            ref_ptr<Ltl> next = ltl->get_lop();
            connect(states, next, cur_state, adj);
            break;
        }
    case OR:
        {
            ref_ptr<Ltl> next_lop = ltl->get_lop();
            ref_ptr<Ltl> next_rop = ltl->get_rop();
            connect(states, next_lop, cur_state, adj);
            connect(states, next_rop, cur_state, adj);
            break;
        }
    case U:
        {
            ref_ptr<Ltl> next_lop = ltl->get_lop();
            ref_ptr<Ltl> next_rop = ltl->get_rop();
            connect(states, next_lop, cur_state, adj);
            connect(states, next_rop, cur_state, adj);
            std::string fl, fr;
            next_lop->to_string(fl);
            next_rop->to_string(fr);
            if (states[cur_state].find(ins) != states[cur_state].end()) {
                if (states[cur_state].find(fr) == states[cur_state].end()) {
                    for (int i = 0; i < states.size(); i++) {
                        if (states[i].find(ins) == states[i].end()) {
                            adj.erase(i);
                        }
                    }
                }
            } else {
                if (states[cur_state].find(fl) != states[cur_state].end()) {
                    for (int i = 0; i < states.size(); i++) {
                        if (states[i].find(ins) != states[i].end()) {
                            adj.erase(i);
                        }
                    }
                }
            }
            
            break;
        }
    case X:
        {
            ref_ptr<Ltl> next = ltl->get_lop();
            connect(states, next, cur_state, adj);
            if (states[cur_state].find(ins) != states[cur_state].end()) {
                std::string f;
                next->to_string(f);
                for (int i = 0; i < states.size(); i++) {
                    if (states[i].find(f) == states[i].end()) {
                        adj.erase(i);
                    }
                }
            }
            
            break;
        }
    }
}

void accept(std::vector <std::set <std::string>> &states, ref_ptr<Ltl> &ltl, Automaton &buchi, int &set_idx) {
    std::string ins;
    ltl->to_string(ins);
    switch (ltl->kind())
    {
    case X:
    case NOT:
        {
            ref_ptr<Ltl> next = ltl->get_lop();
            accept(states, next, buchi, set_idx);
            break;
        }
    case OR:
        {
            ref_ptr<Ltl> next_lop = ltl->get_lop();
            ref_ptr<Ltl> next_rop = ltl->get_rop();
            accept(states, next_lop, buchi, set_idx);
            accept(states, next_rop, buchi, set_idx);
            break;
        }
    case U:
        {
            ref_ptr<Ltl> next_lop = ltl->get_lop();
            ref_ptr<Ltl> next_rop = ltl->get_rop();
            accept(states, next_lop, buchi, set_idx);
            accept(states, next_rop, buchi, set_idx);
            std::string fr;
            next_rop->to_string(fr);
            for (int i = 0; i < states.size(); i++) {
                if (states[i].find(ins) == states[i].end()
                    || states[i].find(fr) != states[i].end()) {
                    buchi.mark_accept(set_idx, i);
                }
            }
            set_idx++;
            break;
        }
    }
}

int
main(int argc, char *argv[]) {
    std::string str_ltl;
    for (int i = 1; i < argc; ++i) {
        str_ltl.append(argv[i]);
    }
    Parser parser;
    ref_ptr<Ltl> ltl = parser.parse(str_ltl.c_str());
    std::set<std::string> atoms;
    init(ltl, atoms, 0);

    int ats = 1 << atoms.size();
    // если атомов больше 31, то помянем T_T
    
    std::vector<std::set<std::string>> states;
    
    for (int i = 0; i < ats; i++) {
        std::set <std::string> init_closure = {"true", "!false"};
        int cur_atom = 0;
        for (auto j: atoms) {
            if ((1 << cur_atom) & i) {
                init_closure.insert(j);
            }
            cur_atom++;
        }
        states.push_back(init_closure);
        closure(states, ltl, states.size() - 1);
    }
    
    Automaton buchi(states.size());
    std::unique_ptr<Automaton> buchiptr;
    std::set <int> adj;
    for (int i = 0; i < states.size(); i++) {
        adj.insert(i);
    }
    std::string ins;
    ltl->to_string(ins);
    for (int i = 0; i < states.size(); i++) {
        if (states[i].find(ins) != states[i].end()) {
            buchi.mark_init(i);
        }
        std::set <int> cur_adj = adj;
        connect(states, ltl, i, cur_adj);
        for (auto j: cur_adj) {
            buchi.add_transition(i, j);
        }
    }
    int acc = 0;
    accept(states, ltl, buchi, acc);
    buchi.write_to(stdout);
    return 0;
}
