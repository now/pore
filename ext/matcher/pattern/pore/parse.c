/*
 * contents: Regular expression parser.
 *
 * Copyright © 2005 Nikolai Weibull <work@rawuncut.elitemail.org>
 */

#include <ruby.h>
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

#include <encoding/character/utf-8/unicode.h>

#include "mempool.h"
#include "ast.h"
#include "parse.h"
#include "private.h"

/*¶ \section{Parsing {\NRE}s to {\AST}s}

We now have all the tools necessary to create an \AST\ from an \NRE.  What
remains is to actually perform the parsing of an \NRE\ resulting in an \AST. */


/*¶ \subsection{Keeping track of parser context.}

Before we get into the actual parsing code, we need a few data structures and
functions that will keep track of our parser’s context.  The context will need
to keep track of memory allocation, active rules, input sources, nesting level
of brackets, and what \C{id}s to assign to leaf nodes in the \AST. */


/*¶ The first thing that we’ll define is a way of keeping track of input: */

typedef struct _InputSource InputSource;

struct _InputSource {
        InputSource *next;
        const char *base;
        const char *iter;
};

/*¶ Input sources are kept on a stack, as rules introduce a new input
source in the regular expression into which they expand.  An input source is a
base pointer to a \CLang\ string and an iterator that points into this string,
demarcating the current point in the input of the parser context.  Once the
iterator reaches the end of the string, the input source will be popped off of
the stack and the parser will resume parsing the one below it.  Once all input
sources have been popped off of the stack, the parsing will end. */


/*¶ Now, then, we can define our parser context: */

typedef struct _ParserContext ParserContext;

struct _ParserContext {
        MemPool *pool;
        VALUE rules;
        InputSource *input;
        int n_brackets;
        int n_square_brackets;
        int next_submatch_id;
        int next_id;
};

/*¶ A \C{ParserContext} thus keeps track of the information mentioned in the
beginning of this subsection.  The \C{rules} field is a Ruby value that should
point to a Ruby Hash, where the keys are the names of the rules and the values
are their expansions.  Again, none of the four last fields will ever be
negative, but we will not use the \C{unsigned} storage specifier, as we limit
its use to bit||fields. */


/*¶ It’s time to write some functions that act upon a parser||context. We need
a way to push and pop input sources in our context, so let’s begin with two
functions that does just that: */

static void
context_push_input(ParserContext *context, const char *text)
{
        assert(context != NULL);

        InputSource *input = ALLOC(InputSource);

        input->next = context->input;
        input->base = input->iter = text;

        context->input = input;
}


static void
context_pop_input(ParserContext *context)
{
        assert(context != NULL);
        assert(context->input != NULL);

        InputSource *t = context->input;
        context->input = context->input->next;
        free(t);
}

/*¶ Nothing fancy going on here.  Next, we need a way to report the position of
the current input iterator.  This will be instrumental in reporting parsing
errors. */


static inline long
context_pos(ParserContext *context)
{
        assert(context != NULL);

        return utf_pointer_to_offset(context->input->base, context->input->iter);
}

/*¶ We will not be covering the \UTF-8 functions (such as
\C{utf_pointer_to_offset} above) in any detail. */


/*¶ It is useful to separate out the test for more input in a function, in case
the method used to determine this will change: */

static inline bool
context_has_more_input(ParserContext *context)
{
        assert(context != NULL);

        return *context->input->iter != '\0';
}

/*¶ For now it’s a simple test for a \C{NUL} in the input string.  \C{NUL},
the character at position $0$ in the \ASCII\ character set, is used in \CLang\
to represent the end of a string. */


/*¶ Another important function on contexts is to get the next character in the
input.  But before we can write it, we need a macro for raising an error in
Ruby.  It will, for example, be an error to try to get another character from
the input, if there is no more input to be gotten.  Before we can raise an
error in Ruby, we must make sure that any memory that has been allocated for
this parse is freed. */

#define RAISE(context, error, ...) do { \
        mem_pool_free((context)->pool); \
        rb_raise((error), __VA_ARGS__); \
} while (0)


/*¶ This macro shows the standard trick for getting the expansion of a macro
treated as a single statement, by wrapping it in a \C{do}\dots\C{while}||loop
that only iterates once.  Any self||respecting compiler will optimize away the
unnecessary code that would otherwise be generated for such a loop. */


static inline unichar
context_next(ParserContext *context)
{
        if (!context_has_more_input(context))
                RAISE(context, g_ePatternError,
                      "end of input reached unexpectedly");

        unichar c = utf_char(context->input->iter);
        context->input->iter = utf_next(context->input->iter);
        return c;
}


/*¶ The \C{context_next} function isn’t very useful by itself and we would like
some utility functions that’ll simplify our life immensely.  Examples of tests
that we want to do on the input include:

\startitemize
  \item Checking if the input iterator is over a given character

  \item Checking if the input iterator is over any of a range of characters

  \item Advancing the input iterator to the next character, if it is a given character

  \item Backing up the input iterator to the previous character

  \item Advancing the input iterator while a condition holds for the characters
    under it
\stopitemize

*/

/*¶ The first test listed above is very simple to implement: */

static inline bool
context_is_at(ParserContext *context, unichar c)
{
        assert(context != NULL);

        return context_has_more_input(context) &&
                utf_char(context->input->iter) == c;
}

/*¶ The second isn’t much harder: */

static inline bool
context_is_at_any_of(ParserContext *context, const char *cs)
{
        assert(context != NULL);
        assert(cs != NULL);

        return context_has_more_input(context) &&
                utf_char_index(cs, utf_char(context->input->iter)) >= 0;
}


/*¶ The third is a bit more complicated.  We begin by checking if we are at the
given character and if we’re not, we simply fail.  If we are, it’s a simple
matter of advancing the input iterator to the next character and return
successfully. */

static inline bool
context_eat(ParserContext *context, unichar c)
{
        assert(context != NULL);

        if (!context_is_at(context, c))
                return false;

        context->input->iter = utf_next(context->input->iter);
        return true;
}

/*¶ This test will be very useful, as we are able to make a decision on what to
do next by trying to {\em eat} a specific character and, if eaten successfully,
we can take appropriate action.  If not, we can allow a later rule in the
parsing deal with what is under the input iterator. */


/*¶ In some cases we may actually need to backtrack out of a previous parse.
To do this without erring, we must restore our iterator: */

static inline void
context_uneat(ParserContext *context)
{
        assert(context != NULL);
        assert(context->input->iter > context->input->base);

        context->input->iter = utf_prev(context->input->iter);
}


/*¶ The final point of the list presented above is the most complex to
implement: \CDef[context_eat_while] */

static const char *
context_eat_while(ParserContext *context, bool (*test)(unichar))
{
        assert(context != NULL);
        assert(test != NULL);

        const char *begin = context->input->iter;

        while (context_has_more_input(context) &&
               test(utf_char(context->input->iter)))
                context->input->iter = utf_next(context->input->iter);

        return begin;
}

/*¶ We return the beginning of the eaten range so that it may easily be
extracted from the string.  That is, the part of the string that was eaten will
be \C{begin} through \C{context->input->iter - begin}.  The region of the
string can be copied like so:

\startC
char sub[context->input->iter - begin + 1];
strncpy(sub, begin, context->input->iter - begin);
sub[context->input->iter - begin] = '\0';
\stopC

Note how worthless the \C{strncpy} function really is.  A couple of OpenBSD
developers have actually written a whole paper on the subject, see
\cite[Miller99], and provided replacement functions that are now used throughout
the OpenBSD kernel.

A better way to do the copying we did above is: \CDef[memcpy string extraction]

\startC
char sub[context->input->iter - begin + 1];
memcpy(sub, begin, context->input->iter - begin);
sub[context->input->iter - begin] = '\0';
\stopC

On some systems, memcpy will be more heavily optimized than strncpy as well, so
it’s win freakin’ win. Of course, the best thing would be to wrap it all up in
a function that would do the appropriate copying behind the scenes, but we’ll
leave that as an exercise to be done on a rainy day\footnote{Or perhaps a sunny
day, if you’re allergic to sunlight.}. */


/*¶ Anyway, time to return to our actual parser code.  A final utility function
is of some value.  For our bounded iterations, it will be necessary to parse
an integer in the input, so let’s wrap up the \C{context_eat_while} function in
a nice interface for parsing integers: */

static bool
context_eat_integer(ParserContext *context, int *value)
{
        assert(context != NULL);
        assert(value != NULL);

        const char *begin = context_eat_while(context, unichar_isdigit);

        if (context->input->iter == begin)
                return false;

        sscanf(begin, "%d", value);
        return true;
}

/*¶ Using \C{sscanf} is safe here, as \C{begin} must point to the beginning of
an integer, or \C{context->input->iter} would have been equal to it. */



/*¶ \subsection{The parser itself.}

We will implement our parser using the parser context that we have just
written.  A recursive||descent parser will work well for this task and we will
implement it using a separate parsing||function for each construction in the
grammar of our {\NRE}s as defined in
\inbnfgrammar[new syntax:bnf:new regex grammar]. */


/*¶ As the grammar is recursive, we need a forward declaration: */

static ASTNode *parse_union(ParserContext *context);

/*¶ To minimize the number of forward declarations, we’ll begin at the bottom
and work our way upwards in the grammar. */

/*¶ The first parsing||function parses literals.  Actually, “parsing a literal”
is a rather broad task to be defined, so let’s take it step||by||step.  What we
need to do is read one character literal from the input and create an \AST\
node for it.  For most character literals, this is easy, we just read the
character under the input iterator and create a new node with
\CRef[ast_node_literal_new_char].  The problem is, not all character literals
can be expressed as one character in the input, as some characters are used as
operators in our grammar.  Thus, we need an escape method, and the one we’ll
use is to precede such a character with a backslash.  This, then, needs to be
dealt with in the parsing||function.  We begin by writing two functions that
will deal with the characters that follow a backslash.  We will need two, as
such escapes act differently depending on if we are currently inside what we
call an \<escaped string> in our grammar, or not. */

static bool
my_unichar_isident(unichar c)
{
        return unichar_isalnum(c) || c == '_';
}

static bool
my_unichar_isnonident(unichar c)
{
        return !my_unichar_isident(c);
}

static bool
unichar_isnonalpha(unichar c)
{
        return !unichar_isalpha(c);
}

static bool
unichar_isnondigit(unichar c)
{
        return !unichar_isdigit(c);
}

static bool
unichar_isnonlower(unichar c)
{
        return !unichar_islower(c);
}

static bool
unichar_isnonpunct(unichar c)
{
        return !unichar_ispunct(c);
}

static bool
unichar_isnonspace(unichar c)
{
        return !unichar_isspace(c);
}

static bool
unichar_isnonupper(unichar c)
{
        return !unichar_isupper(c);
}


static CharTypePredicate
map_rule_to_predicate(char const * const rule, bool negate)
{
        static struct {
                char const *name;
                CharTypePredicate predicate;
                CharTypePredicate negated_predicate;
        } map[] = {
                { .name = "ident", .predicate = my_unichar_isident, .negated_predicate = my_unichar_isnonident },
                { .name = "alpha", .predicate = unichar_isalpha, .negated_predicate = unichar_isnonalpha },
                { .name = "digit", .predicate = unichar_isdigit, .negated_predicate = unichar_isnondigit },
                { .name = "lower", .predicate = unichar_islower, .negated_predicate = unichar_isnonlower },
                { .name = "punct", .predicate = unichar_ispunct, .negated_predicate = unichar_isnonpunct },
                { .name = "space", .predicate = unichar_isspace, .negated_predicate = unichar_isnonspace },
                { .name = "ws", .predicate = unichar_isspace, .negated_predicate = unichar_isnonspace },
                { .name = "upper", .predicate = unichar_isupper, .negated_predicate = unichar_isnonupper },
        };

        for (unsigned int i = 0; i < lengthof(map); i++)
                if (strcmp(map[i].name, rule) == 0)
                        return negate ? map[i].negated_predicate : map[i].predicate;

        return NULL;
}

static inline unichar
escaped_string_literal(unichar c)
{
        switch (c) {
        case '"': case '\\':
                return c;
        case 'n':
                return '\n';
        case 't':
                return '\t';
        default:
                return UTF_BAD_INPUT_UNICHAR;
        }
}

/*¶ This first function deals with the case of being inside an \<escaped
string>. */


static inline unichar
escaped_literal(unichar c)
{
        switch (c) {
        case '.': case '<': case '[': case '(': case ')': case ']': case '>':
        case '*': case '+': case '?': case '^': case '$': case '|': case '\\':
                return c;
        case 'n':
                return '\n';
        case 't':
                return '\t';
        default:
                return UTF_BAD_INPUT_UNICHAR;
        }
}

/*¶ This second function deals with the normal case of being outside such a
string.  A lot more characters are {\em active} as operators outside an
\<escaped string>, so there are a lot more characters to deal with in this
case.

Both functions have in common that they will return the special character
\C{UTF_BAD_INPUT_UNICHAR} in case the given character isn’t treated specially
after a backslash.  We will use this fact in our next function to report an
error to our user whenever this occurs.  It’s thus an error to include a
backslash in the input when not using it to escape the following character.
*/


/*¶ Thus, let’s write our function for parsing character literals: */

static ASTNode *
parse_literal(ParserContext *context, bool escaped, bool in_string_literal)
{
        assert(context != NULL);

        unichar c;

        if (escaped && context_eat(context, '\\')) {
                unichar (*f)(unichar) = in_string_literal ?
                        escaped_string_literal : escaped_literal;

                if ((c = f(context_next(context))) == UTF_BAD_INPUT_UNICHAR)
                        RAISE(context, g_ePatternError,
                              "unexpected ‘\\’ found at position %ld",
                              context_pos(context));
        } else {
                c = context_next(context);
        }

        return ast_node_literal_new_char(context->pool, c, context->next_id++);
}

/*¶ Not much to note about this function really.  The \C{escaped} parameter
should be set to \C{true} if we are parsing a possibly escaped character
literal.  The last parameter, \C{in_string_literal}, should be set to \C{true}
if we are parsing a character literal inside a string literal, i.e., either
\TypedRegex{<"}\dots\TypedRegex{">} or \TypedRegex{<'}\dots\TypedRegex{'>}. */


/*¶ Speaking of string literals, it’s time that we define their parsing
function: */

static ASTNode *
parse_string_literal(ParserContext *context, unichar c)
{
        assert(context != NULL);

        bool escaped = (c == '\"');
        ASTNode *str = NULL;

        while (!context_eat(context, c)) {
                ASTNode *rc = parse_literal(context, escaped, true);

                str = ast_node_cons_new_or_other(context->pool, str, rc);
        }

        if (!context_eat(context, '>'))
                RAISE(context, g_ePatternError,
                      "expected ‘%c>’ at position %ld",
                      c, context_pos(context));

        return (str != NULL) ? str : ast_node_empty_new(context->pool, -1);
}


/*¶ Our next parsing||function is for the \<rule> rule.  Before we can write
it, though, we need a helper||function that we will use as an argument to
\CRef[context_eat_while]. */

static bool
rule_name_p(unichar c)
{
        return c != '>';
}

/*¶ The idea will be to eat input until we find a \type{>} (or reach the end of
the input, in which case we fail).  Then, we’ll extract the name of the rule
from the input using the method suggested on \at{page}[memcpy page extraction].
This name will then be looked up in the \C{rules} hash of our context and, if
found, its value will be pushed onto the input stack.  We will then try to
parse the new input as an \NRE\ and then returning to our previous input. */

/*¶ Now, then, let’s parse some rules. */

static const char *
eat_rule(ParserContext *context)
{
        const char *begin = context_eat_while(context, rule_name_p);

        if (!context_is_at(context, '>'))
                RAISE(context, g_ePatternError,
                      "expected ‘%c’ at position %ld",
                      '>', context_pos(context) + 1);

        return begin;
}

static void
copy_rule(ParserContext *context, char const * const begin, char *dest)
{
        MEMCPY(dest, begin, char, context->input->iter - begin);
        dest[context->input->iter - begin] = '\0';

        context_eat(context, '>');
}

static ASTNode *
parse_rule(ParserContext *context)
{
        assert(context != NULL);

        long begin_pos = context_pos(context) + 1;
        const char *begin = eat_rule(context);
        char rule[context->input->iter - begin + 1];
        copy_rule(context, begin, rule);

        VALUE expansion = Qnil;
        if (!NIL_P(context->rules))
                expansion = rb_hash_aref(context->rules, rb_str_new2(rule));

        if (NIL_P(expansion)) {
                CharTypePredicate predicate = map_rule_to_predicate(rule, false);
                if (predicate == NULL)
                        RAISE(context, g_ePatternError,
                              "undefined rule ‘%s’ at position %ld", rule, begin_pos);

                return ast_node_literal_new_predicate(context->pool,
                                                      predicate,
                                                      context->next_id++);
        }

        context_push_input(context, StringValuePtr(expansion));
        ASTNode *uni = parse_union(context);
        context_pop_input(context);

        /* TODO: check for errors, and then "stack" them... */

        return uni;
}

static ASTNode *
parse_predicate(ParserContext *context, bool negate)
{
        long begin_pos = context_pos(context) + 1;
        const char *begin = eat_rule(context);
        char rule[context->input->iter - begin + 1];
        copy_rule(context, begin, rule);

        CharTypePredicate predicate = map_rule_to_predicate(rule, negate);

        if (predicate == NULL)
                RAISE(context, g_ePatternError,
                      "undefined predicate ‘%s’ at position %ld", rule, begin_pos);

        return ast_node_literal_new_predicate(context->pool,
                                              predicate,
                                              context->next_id++);
}


/*¶ Next on the list is to parse atoms.  This is where things start to get a
bit complicated, but they’re still manageable.  We begin by defining a function to
mark a node with submatch data, as we will need it for such atoms. */

static void
mark_submatch(MemPool *pool, ASTNode **node, int id)
{
        assert(pool != NULL);
        assert(node != NULL);
        assert(*node != NULL);

        if ((*node)->submatch_id >= 0) {
                ASTNode *empty = ast_node_empty_new(pool, -1);
                ASTNode *cons = ast_node_cons_new(pool, empty, *node);
                cons->n_submatches = (*node)->n_submatches;
                *node = cons;
        }

        (*node)->submatch_id = id;
        ((*node)->n_submatches)++;
}

/*¶ If the node has already been marked with a submatch id, we need to create a
new node that can be marked to include it. */


/*¶ For parsing assertions, what we assert depends on what we have read.  If
\C{c} is a \type{^}, we will be asserting that we are at the beginning of a
line, otherwise we're checking for the end of a line.  If the character is
followed by the same character, we will be asserting that we are at the
beginning or end of the input instead. */

static ASTNode *
parse_assertion(ParserContext *context, unichar c)
{
        Assertion a = (c == '^') ? ASSERTION_BOL : ASSERTION_EOL;

        if (context_eat(context, c))
                a = (c == '^') ? ASSERTION_BOS : ASSERTION_EOS;

        return ast_node_assertion_new(context->pool, a, -1);
}


/*¶ Next, groups will be parsed.  Groups are either addressable or not, and are
delimited by the mirror||character of the beginning delimiter. */

static ASTNode *
parse_group(ParserContext *context, bool addressed, unichar c)
{
        int id = -1;

        if (addressed) {
                id = context->next_submatch_id++;
                context->n_brackets++;
        } else {
                context->n_square_brackets++;
        }

        ASTNode *uni = parse_union(context);

        unichar mc;
        bool success = unichar_mirror(c, &mc);
        assert(success);

        if (!context_eat(context, mc))
                RAISE(context, g_ePatternError,
                      "expected ‘%lc’ at position %ld",
                      mc, context_pos(context) + 1);

        if (addressed) {
                assert(id != -1);
                context->n_brackets--;
                mark_submatch(context->pool, &uni, id);
        } else {
                context->n_square_brackets--;
        }

        return uni;
}

static ASTNode *
parse_range_item(ParserContext *context, bool negate)
{
        unichar begin, end;

        unichar c = context_next(context);
        switch (c) {
        case '\\':
                c = context_next(context);
                switch (c) {
                case '\\':
                case '[':
                case ']':
                case '-':
                case '<':
                case '>':
                        begin = end = c;
                        break;
                case 'n':
                        begin = end = '\n';
                        break;
                case 't':
                        begin = end = '\t';
                        break;
                default:
                        RAISE(context, g_ePatternError,
                              "unexpected ‘%lc’ found at position %ld",
                              c, context_pos(context));
                }
                break;
        case '<':
                return parse_predicate(context, negate);
        default:
                begin = end = c;
                if (context_is_at(context, '-')) {
                        context_next(context);
                        end = context_next(context);
                }
                break;
        }

        return ast_node_literal_new_range(context->pool, begin, end, context->next_id);
}

static int
compare_range_items(const void *a, const void *b)
{
        int a_begin = (*(ASTNode **)a)->data.leaf->data.literal.data.range.begin;
        int b_begin = (*(ASTNode **)b)->data.leaf->data.literal.data.range.begin;

        return a_begin - b_begin;
}

static ASTNode *
parse_range(ParserContext *context)
{
        bool negate = context_eat(context, '^');

        int n_alloced_items = 32;
        int n_items = 0;
        ASTNode **items = ALLOC_N(ASTNode *, n_alloced_items);

        int n_alloced_ctypes = 32;
        int n_ctypes = 0;
        ASTNode **ctypes = ALLOC_N(ASTNode *, n_alloced_ctypes);

        int n_alloced_negated_ctypes = 32;
        int n_negated_ctypes = 0;
        CharTypePredicate *negated_ctypes = ALLOC_N(CharTypePredicate,
                                                    n_alloced_negated_ctypes);

        while (!context_is_at(context, ']')) {
                ASTNode *item = parse_range_item(context, negate);
                assert(item->type == AST_NODE_LEAF);
                assert(item->data.leaf->type == LEAF_LITERAL);
                if (item->data.leaf->data.literal.type == LITERAL_TYPE_PREDICATE) {
                        if (negate) {
                                if (n_negated_ctypes == n_alloced_negated_ctypes) {
                                        n_alloced_negated_ctypes *= 2;
                                        CharTypePredicate *tmp = REALLOC_N(negated_ctypes,
                                                                           CharTypePredicate,
                                                                           n_alloced_negated_ctypes);
                                        negated_ctypes = tmp;
                                }
                                negated_ctypes[n_negated_ctypes++] = item->data.leaf->data.literal.data.is_ctype;
                        } else {
                                if (n_ctypes == n_alloced_negated_ctypes) {
                                        n_alloced_negated_ctypes *= 2;
                                        ASTNode **tmp = REALLOC_N(ctypes,
                                                                  ASTNode *,
                                                                  n_alloced_negated_ctypes);
                                        ctypes = tmp;
                                }
                                ctypes[n_ctypes++] = item;
                        }
                } else {
                        if (n_items == n_alloced_items) {
                                n_alloced_items *= 2;
                                ASTNode **tmp = REALLOC_N(items, ASTNode *, n_alloced_items);
                                items = tmp;
                        }
                        items[n_items++] = item;
                }
        }

        if (!context_eat(context, ']') || !context_eat(context, '>')) {
                free(negated_ctypes);
                free(ctypes);
                free(items);
                RAISE(context, g_ePatternError,
                      "expected ‘]>’ at position %ld",
                      context_pos(context));
        }

        if (n_items == 0 && n_ctypes == 0 && !negate) {
                free(negated_ctypes);
                free(ctypes);
                free(items);
                RAISE(context, g_ePatternError,
                      "empty range at position %ld",
                      context_pos(context));
        }

        CharTypePredicate *dup = POOL_ALLOC_N(context->pool, CharTypePredicate, n_negated_ctypes + 1);
        for (int i = 0; i < n_negated_ctypes; i++)
                dup[i] = negated_ctypes[i];
        dup[n_negated_ctypes] = NULL;
        free(negated_ctypes);
        negated_ctypes = dup;

        ASTNode *ranges = NULL;

        for (int i = 0; i < n_ctypes; i++)
                ranges = ast_node_cons_new_or_other(context->pool, ranges, ctypes[i]);
        free(ctypes);

        if (negate)
                qsort(items, n_items, sizeof(ASTNode *), compare_range_items);

        int current_begin = 0;
        int current_end = 0;

        for (int i = 0; i < n_items; i++) {
                if (negate) {
                        int begin = items[i]->data.leaf->data.literal.data.range.begin;
                        int end = items[i]->data.leaf->data.literal.data.range.end;
                        if (begin < current_end) {
                                /* This item overlaps with a previous one. */
                                current_end = MAX(end + 1, current_end);
                                continue;
                        } else {
                                current_end = begin - 1;
                                if (current_end >= current_begin) {
                                        items[i]->data.leaf->data.literal.data.range.begin = current_begin;
                                        items[i]->data.leaf->data.literal.data.range.end = current_end;
                                        current_begin = current_end = end + 1;
                                } else {
                                        current_begin = current_end = end + 1;
                                        continue;
                                }
                        }
                }

                items[i]->data.leaf->id = context->next_id;
                items[i]->data.leaf->data.literal.negated_ctypes = negated_ctypes;

                ranges = ast_node_union_new_or_other(context->pool, ranges, items[i]);
        }
        free(items);

        if (negate) {
                ASTNode *range = ast_node_literal_new_range(context->pool,
                                                            current_begin,
                                                            MAXUNICHAR,
                                                            context->next_id);
                range->data.leaf->data.literal.negated_ctypes = negated_ctypes;

                ranges = ast_node_union_new_or_other(context->pool, ranges, range);
        }

        context->next_id++;

        return ranges;
}

/*¶ There’s not much that needs explanation, really. */

/*¶ The final set of atoms to be parsed is those that begin with a
\TypedRegex{<}.  These are either string literals, escaped or not, or a
range: */

static ASTNode *
parse_special(ParserContext *context)
{
        unichar c = context_next(context);
        switch (c) {
        case '"':
        case '\'':
                return parse_string_literal(context, c);
        case '[':
                return parse_range(context);
        default:
                context_uneat(context);
                return parse_rule(context);
                /*
                RAISE(context, g_ePatternError,
                      "unexpected ‘%lc’ found at position %ld",
                      c, context_pos(context));
                      */
        }
}

/*¶ Now that we have a bunch of helper||functions for parsing various kinds of
atoms, let’s write a function to invoke them as appropriate: */

static ASTNode *
parse_atom(ParserContext *context)
{
        assert(context != NULL);

        if (!context_has_more_input(context) ||
            context_is_at_any_of(context, "|])"))
                return ast_node_empty_new(context->pool, -1);

        unichar c = context_next(context);
        switch (c) {
        case '.':
                return ast_node_literal_new_range(context->pool, 0, MAXUNICHAR,
                                                  context->next_id++);
        case '^':
        case '$':
                return parse_assertion(context, c);
        case '[':
        case '(':
                return parse_group(context, c == '(', c);
        case '<':
                return parse_special(context);
        default:
                context_uneat(context);
                return parse_literal(context, true, false);
        }
}

/*¶ If we don’t have any more input to read from or if we are at any of a set
of delimiters, what we want is a match for the empty string, e.g.,
\TypedRegex{a|} and \TypedRegex{|a} should match the language
$\{a, \epsilon\}$.  Likewise, the grouped versions of such a construction
should do the same. */


/*¶ By far the most complex part of our grammar to parse is \<quantifier>.  It
asks us to try a range of possible \<range> constructs, while we have to make
sure that any ill||formed ones don’t get through: */

static bool
parse_bounded_quantifier(ParserContext *context, int *min, int *max)
{
        if (context_eat(context, '>'))
                goto missing_integer;

        long begin_pos = context_pos(context);
        bool read_min = false;

        if (context_is_at(context, ',')) {
                *min = 0;
        } else {
                if (!context_eat_integer(context, min))
                        return false;

                read_min = true;
        }

        if (context_is_at(context, '>')) {
                *max = *min;
        } else if (context_eat(context, ',')) {
                   if (context_is_at(context, '>')) {
                           if (read_min)
                                   *max = -1;
                           else
                                   goto missing_integer;
                   } else if (!context_eat_integer(context, max)) {
                           goto missing_integer;
                   }
        }

        if (!context_eat(context, '>'))
                RAISE(context, g_ePatternError,
                      "expected ‘>’ at position %ld",
                      context_pos(context) + 1);

        if (*max != -1 && *min > *max)
                RAISE(context, g_ePatternError,
                      "lower limit is greater than upper in bounded "
                      "repetition at position %ld", begin_pos);

        return true;

missing_integer:

        RAISE(context, g_ePatternError,
                "expected decimal integer at position %ld",
                context_pos(context) + 1);
}

/*¶ Horrendous.  It does show a good use for the \C{goto} statement, though.  A
weird effect of our grammar is that we don’t allow the potential constructions

\startbnfgrammar[]
<range>: "<" ">" | "<" "," ">"
\stopbnfgrammar

Having these constructions would actually simplify the code a lot, as there
would be far less ways in which the user could misconstruct a range. */


/*¶ The next step is to parse the rest of the quantifiers.  It’s more or less
a task of setting the correct lower and upper boundaries and checking that they
make sense: */

static ASTNode *
parse_quantifier(ParserContext *context, ASTNode *atom)
{
        int min, max;

        switch (context_next(context)) {
        case '*': min = 0; max = -1; break;
        case '+': min = 1; max = -1; break;
        case '?': min = 0; max =  1; break;
        case '<':
                if (!parse_bounded_quantifier(context, &min, &max)) {
                        context_uneat(context);
                        return atom;
                }
                break;
        default:
                assert(false);
                return NULL;
        }

        bool minimal = context_eat(context, '?');

        if (min == 0 && max == 0)
                return ast_node_empty_new(context->pool, -1);

        return ast_node_iter_new(context->pool, atom, min, max, minimal);
}

/*¶ Note that if we fail to parse a bounded quantifier after seeing a \type{<}
we must uneat it and return the atom, as the \type{<} may actually be part of a
rule that starts right after the atom, not a quantifier as we initially
thought. */


static ASTNode *
parse_piece(ParserContext *context)
{
        ASTNode *atom = parse_atom(context);

        if (context_is_at_any_of(context, "*+?<"))
                return parse_quantifier(context, atom);

        return atom;
}

/*¶ That concludes the parsing of pieces, i.e., an atom, optionally followed by
a quantifier. */


/*¶ The next grammar rule to parse is \<concat>: */

static ASTNode *
parse_cons(ParserContext *context)
{
        ASTNode *piece = parse_piece(context);

        if ((context_is_at(context, ']') && context->n_square_brackets == 0) ||
            (context_is_at(context, ')') && context->n_brackets == 0))
                RAISE(context, g_ePatternError,
                      "unexpected ‘%lc’ found at position %ld",
                      *context->input->iter, context_pos(context) + 1);

        if (context_is_at_any_of(context, "])|") ||
            !context_has_more_input(context))
                return piece;

        return ast_node_cons_new(context->pool, piece, parse_cons(context));
}


/*¶ The final parsing||function is for unions: */

static ASTNode *
parse_union(ParserContext *context)
{
        ASTNode *cons = parse_cons(context);

        if (!context_eat(context, '|'))
                return cons;

        return ast_node_union_new(context->pool, cons, parse_union(context));
}


/*¶ Now that we’ve written all the necessary parsing functions, let’s write the
function that will set up the parsing||context and start off the parsing for
us: */

HIDDEN ASTNode *
pattern_parse(MemPool *pool, VALUE rbpattern, VALUE rules,
              int *n_nodes, int *n_submatches)
{
        ParserContext context = {
                .pool = pool,
                .rules = rules,
                .input = NULL,
                .n_brackets = 0,
                .n_square_brackets = 0,
                .next_submatch_id = 1,
                .next_id = 0
        };

        char *pattern = StringValuePtr(rbpattern);

        if (!utf_isvalid(pattern))
                RAISE(&context, g_ePatternError,
                      "invalid UTF-8-encoded text in pattern");

        context_push_input(&context, pattern);
        ASTNode *tree = parse_union(&context);
        context_pop_input(&context);

        mark_submatch(context.pool, &tree, 0);

        *n_nodes = context.next_id;
        *n_submatches = context.next_submatch_id;

        return tree;
}
