#include <stdint.h>

#ifdef DEBUGGING
#include <stdio.h>
#endif


#define STACK_SIZE 20


/*
  Grammar:

  VAR = 't' | 'a' | 'b' | 'c'
  NUM = { '0'..'9' }

  exp0 = { ':' VAR = exp6 ';' } exp6
  exp6 = exp7 { '|' exp7}
  exp7 = exp8 { '^' exp8}
  exp8 = exp9 { '&' exp9}
  exp9 = exp10 ('==' | '!=') exp10
  exp10 = exp11 ('<' | '<=' | '>' | '>=') exp11
  exp11 = exp12 ('<<' | '>>') exp12
  exp12 = exp13 { ('+' | '-') exp13 }
  exp13 = exp15 { ('*' | '/' | '%') exp15 }
  exp15 = [ '+' | '-' | '!' | '~' ] exp16
  exp16 =
      VAR
    | NUM
    | '(' exp6 ')'

*/

enum compiled_exp_op {
  OP_CONSTANT, OP_VAR,
  OP_NEGATE, OP_BOOLEAN_NOT, OP_LOGICAL_NOT,
  OP_MULTIPLY, OP_DIVIDE, OP_MODULO,
  OP_PLUS, OP_MINUS,
  OP_LEFT_SHIFT, OP_RIGHT_SHIFT,
  OP_LESS_THAN, OP_LESS_THAN_OR_EQUAL, OP_GREATER_THAN, OP_GREATER_THAN_OR_EQUAL,
  OP_EQUAL, OP_NOT_EQUAL,
  OP_LOGICAL_AND,
  OP_LOGICAL_XOR,
  OP_LOGICAL_OR,
  OP_ASSIGN
};

enum compiled_exp_var {
  VAR_T = 0, VAR_A = 1, VAR_B = 2, VAR_C = 3,
  MAX_VAR
};
int32_t variable_values[MAX_VAR];


int32_t eval_stack[STACK_SIZE];
uint16_t eval_stack_pointer = 0;

static void
stack_push(int32_t val)
{
  /*
    ToDo: We could actually avoid this stack overflow check.
    Instead, we could pre-compute the stack usage of a compiled expression.
    And just check once at start of eval (or even check at compile) that no
    overflow will occur.
  */
  if (eval_stack_pointer < STACK_SIZE)
    eval_stack[eval_stack_pointer++] = val;
}


static int32_t
stack_pop(void)
{
  if (eval_stack_pointer > 0)
    --eval_stack_pointer;
  return eval_stack[eval_stack_pointer];
}


int32_t
eval_compiled(uint8_t compiled_exp[], uint16_t compiled_len, int32_t vars[])
{
  uint16_t pc;
  int32_t arg1, arg2;

  pc = 0;
  eval_stack_pointer = 0;
  while (pc < compiled_len) {
    uint8_t op = compiled_exp[pc++];
    switch (op) {
    case OP_CONSTANT:
      stack_push((uint32_t)compiled_exp[pc] |
                 ((uint32_t)compiled_exp[pc+1] << 8) |
                 ((uint32_t)compiled_exp[pc+2] << 16) |
                 ((uint32_t)compiled_exp[pc+3] << 24));
      pc += 4;
      break;
    case OP_VAR:
      stack_push(vars[compiled_exp[pc++]]);
      break;
    case OP_NEGATE:
      stack_push(-stack_pop());
      break;
    case OP_BOOLEAN_NOT:
      stack_push(!stack_pop());
      break;
    case OP_LOGICAL_NOT:
      stack_push(~(uint32_t)stack_pop());
      break;
    case OP_MULTIPLY:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 * arg2);
      break;
    case OP_DIVIDE:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push((arg2 == 0) ? 0 : arg1 / arg2);
      break;
    case OP_MODULO:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push((arg2 == 0) ? 0 : arg1 % arg2);
      break;
    case OP_PLUS:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 + arg2);
      break;
    case OP_MINUS:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 - arg2);
      break;
    case OP_LEFT_SHIFT:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 << arg2);
      break;
    case OP_RIGHT_SHIFT:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 >> arg2);
      break;
    case OP_LESS_THAN:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 < arg2);
      break;
    case OP_LESS_THAN_OR_EQUAL:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 <= arg2);
      break;
    case OP_GREATER_THAN:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 > arg2);
      break;
    case OP_GREATER_THAN_OR_EQUAL:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 >= arg2);
      break;
    case OP_EQUAL:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 == arg2);
      break;
    case OP_NOT_EQUAL:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 != arg2);
      break;
    case OP_LOGICAL_AND:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 & arg2);
      break;
    case OP_LOGICAL_XOR:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 ^ arg2);
      break;
    case OP_LOGICAL_OR:
      arg2 = stack_pop();
      arg1 = stack_pop();
      stack_push(arg1 | arg2);
      break;
    case OP_ASSIGN:
      vars[compiled_exp[pc++]] = stack_pop();
      break;
    default:
      /* Should not happen... */
#ifdef DEBUGGING
      fprintf(stderr, "ERROR unknown op: %u\n", (unsigned)op);
#endif
      ;
    }
  }
  return stack_pop();
}


const char *exp_to_parse;
uint8_t *compile_next_loc;
uint16_t compile_remain_loc;

static void parse_error(void);

void
emit_op(uint8_t op, int32_t arg)
{
  if (compile_remain_loc < 1) {
    parse_error();
    return;
  }
  *compile_next_loc++ = op;
  --compile_remain_loc;

  if (op == OP_VAR || op == OP_ASSIGN) {
    if (compile_remain_loc < 1) {
      parse_error();
      return;
    }
    *compile_next_loc++ = (uint8_t)arg;
    --compile_remain_loc;
  } else if (op == OP_CONSTANT) {
    if (compile_remain_loc < 4) {
      parse_error();
      return;
    }
    *compile_next_loc++ = arg;
    *compile_next_loc++ = (uint32_t)arg >> 8;
    *compile_next_loc++ = (uint32_t)arg >> 16;
    *compile_next_loc++ = (uint32_t)arg >> 24;
    compile_remain_loc -= 4;
  }
}


uint8_t current_token;
int32_t current_token_value;
uint8_t got_parser_error;
const char *parser_error_location;


void
next_token(void)
{
  const char *p = exp_to_parse;
  char c;

  /* Skip white space. */
  while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')
    ++p;
  c = *p++;
#ifdef DEBUGGING
  fprintf(stderr, "  current char: %c%s\n", (c ? c : '\\'), (c ? "" : "0"));
#endif
  if (c >= '0' && c <= '9') {
    /* Number. */
    uint32_t val = c - '0';
    while (*p >= '0' && *p <= '9')
      val = (val * 10) + (uint32_t)(*p++ - '0');
    current_token = '0';
    current_token_value = val;
  } else if (c == '=') {
    if (*p == '=') {                            /* Comparison == */
      ++p;
      current_token = '=';
    } else {                                    /* Assignment = */
      current_token = 'A';
    }
  } else if (c == '<') {
    if (*p == '=') {                            /* Under-or-equal <= */
      ++p;
      current_token = 'U';
    } else if (*p == '<') {                     /* Left shift << */
      ++p;
      current_token = 'L';
    } else {                                    /* Less-than < */
      current_token = '<';
    }
  } else if (c == '>') {
    if (*p == '=') {                            /* Greater-or-equal >= */
      ++p;
      current_token = 'G';
    } else if (*p == '>') {                     /* Right shift >> */
      ++p;
      current_token = 'R';
    } else {                                    /* Greater-than > */
      current_token = '>';
    }
  } else if (c == '!') {
    if (*p == '=') {                            /* Not equals != */
      ++p;
      current_token = '#';
    } else {                                    /* Boolean not ! */
      current_token = '!';
    }
  } else {
    switch (c) {
    case '\0':                                  /* End-of-file. */
    case '+':
    case '-':
    case ':':
    case ';':
    case '|':
    case '^':
    case '&':
    case '*':
    case '/':
    case '%':
    case '!':
    case '~':
    case '(':
    case ')':
      current_token = c;
      break;
    case 't':
      current_token = 'V';
      current_token_value = VAR_T;
      break;
    case 'a':
      current_token = 'V';
      current_token_value = VAR_A;
      break;
    case 'b':
      current_token = 'V';
      current_token_value = VAR_B;
      break;
    case 'c':
      current_token = 'V';
      current_token_value = VAR_C;
      break;
    default:
      /* Parse error, unknown character. */
      current_token = 'E';
      break;
    }
  }

  exp_to_parse = p;
#ifdef DEBUGGING
  fprintf(stderr, "NEXT TOKEN: %c%s\n",
          (current_token ? current_token : '('),
          (current_token ? "" : "EOF)"));
#endif
}


static void
parse_error(void)
{
  if (!got_parser_error) {
    got_parser_error = 1;
    parser_error_location = exp_to_parse;
  }
}


static uint8_t
parse_accept(uint8_t tok)
{
  if (current_token == tok) {
    if (current_token != '\0')                  /* Don't move past EOF */
      next_token();
    return 1;
  }
  return 0;
}


static uint8_t
parse_expect(uint8_t tok)
{
  if (parse_accept(tok))
    return 1;
  parse_error();
  return 0;
}


void parse_exp6(void);

void
parse_exp16(void)
{
  int32_t tok_val = current_token_value;
  if (parse_accept('V')) {
    emit_op(OP_VAR, tok_val);
  } else if (parse_accept('0')) {
    emit_op(OP_CONSTANT, tok_val);
  } else {
    if (parse_expect('(')) {
      parse_exp6();
      parse_expect(')');
    }
  }
}


void
parse_exp15(void)
{
  if (parse_accept('+')) {
    parse_exp16();
    /* Unary '+' is a no-operation, like + 5. */
  } else if (parse_accept('-')) {
    parse_exp16();
    emit_op(OP_NEGATE, 0);
  } else if (parse_accept('!')) {
    parse_exp16();
    emit_op(OP_BOOLEAN_NOT, 0);
  } else if (parse_accept('~')) {
    parse_exp16();
    emit_op(OP_LOGICAL_NOT, 0);
  } else {
    parse_exp16();
  }
}


void
parse_exp13(void)
{
  parse_exp15();
  while (current_token == '*' || current_token == '/' || current_token == '%') {
    uint8_t op = (current_token == '*' ? OP_MULTIPLY :
                  (current_token == '/' ? OP_DIVIDE : OP_MODULO));
    next_token();
    parse_exp15();
    emit_op(op, 0);
  }
}


void
parse_exp12(void)
{
  parse_exp13();
  while (current_token == '+' || current_token == '-') {
    uint8_t op = (current_token == '+' ? OP_PLUS : OP_MINUS);
    next_token();
    parse_exp13();
    emit_op(op, 0);
  }
}


void
parse_exp11(void)
{
  parse_exp12();
  if (current_token == 'L' || current_token == 'R') {
    uint8_t op = (current_token == 'L' ? OP_LEFT_SHIFT : OP_RIGHT_SHIFT);
    next_token();
    parse_exp12();
    emit_op(op, 0);
  }
}


void
parse_exp10(void)
{
  parse_exp11();
  if (current_token == '<' || current_token == 'U' ||
      current_token == '>' || current_token == 'G') {
    uint8_t op;
    switch (current_token) {
    case '<': op = OP_LESS_THAN; break;
    case 'U': op = OP_LESS_THAN_OR_EQUAL; break;
    case '>': op = OP_GREATER_THAN; break;
    default:  op = OP_GREATER_THAN_OR_EQUAL; break;
    }
    next_token();
    parse_exp11();
    emit_op(op, 0);
  }
}


void
parse_exp9(void)
{
  parse_exp10();
  if (current_token == '=' || current_token == '#') {
    uint8_t op = (current_token == '=' ? OP_EQUAL : OP_NOT_EQUAL);
    next_token();
    parse_exp10();
    emit_op(op, 0);
  }
}


void
parse_exp8(void)
{
  parse_exp9();
  while (current_token == '&') {
    next_token();
    parse_exp9();
    emit_op(OP_LOGICAL_AND, 0);
  }
}


void
parse_exp7(void)
{
  parse_exp8();
  while (current_token == '^') {
    next_token();
    parse_exp8();
    emit_op(OP_LOGICAL_XOR, 0);
  }
}


void
parse_exp6(void)
{
  parse_exp7();
  while (current_token == '|') {
    next_token();
    parse_exp7();
    emit_op(OP_LOGICAL_OR, 0);
  }
}


void
parse_exp0(void)
{
  while (parse_accept(':')) {
    uint8_t tok_val = current_token_value;
    if (parse_expect('V')) {
      if (parse_expect('A')) {
        parse_exp6();
        if (parse_expect(';'))
          emit_op(OP_ASSIGN, tok_val);
      }
    }
  }
  parse_exp6();
}


void
start_parse()
{
  next_token();
  parse_exp0();
  parse_expect('\0');
}


uint16_t
parse_exp(const char *exp, uint8_t *buffer, uint16_t buffer_len,
          uint16_t *error_pos)
{
  exp_to_parse = exp;
  compile_next_loc = buffer;
  compile_remain_loc = buffer_len;
  got_parser_error = 0;

  start_parse();
  if (got_parser_error) {
    if (error_pos)
      *error_pos = parser_error_location - exp;
    return 0;
  } else {
    if (error_pos)
      *error_pos = 0;
    return compile_next_loc - buffer;
  }
}


#ifdef TESTING

#include <stdlib.h>
#include <stdio.h>

int
main(int argc, char *argv[])
{
  int i;

  for (i = 1; i < argc; ++i) {
    uint16_t error_pos;
    uint8_t compiled_code[256];
    uint16_t compiled_size;
    int32_t t;

    printf("Parsing: %s\n", argv[i]);
    compiled_size = parse_exp(argv[i], compiled_code, sizeof(compiled_code), &error_pos);
    if (!compiled_size) {
      printf("Parse error at position %u\n", (unsigned)error_pos);
      continue;
    }
    for (t = 0; t < 10; ++t) {
      int32_t val;
      uint16_t i;

      variable_values[0] = t;
      for (i = 1; i < MAX_VAR; ++i)
        variable_values[i] = 0;
      val = eval_compiled(compiled_code, compiled_size, variable_values);
      printf("%2ld  %10ld\n", (long)t, (long)val);
    }
  }

  return 0;
}
#endif
