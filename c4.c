// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

char *p, *lp, // current position in source code
     *data, *sdata;   // data/bss pointer

int *e, *le, *se,  // current position in emitted code
    *id,      // currently parsed indentifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug,    // print executed instructions
    zcode;    // emit zcode

// tokens and classes (operators last and in precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// opcodes
enum { LEA ,LGA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,ARG ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT };

// types
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

next()
{
  char *pp;

  while (tk = *p) {
    ++p;
    if (tk == '\n') {
      if (src) {
        printf("%d: %.*s", line, p - lp, lp);
        lp = p;
        while (le < e) {
          printf("%8.4s", &"LEA ,LGA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,ARG ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
        }
      }
      ++line;
    }
    else if (tk == '#') {
      while (*p != 0 && *p != '\n') ++p;
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
      pp = p - 1;
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++;
      tk = (tk << 6) + (p - pp);
      id = sym;
      while (id[Tk]) {
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return 0; }
        id = id + Idsz;
      }
      id[Name] = (int)pp;
      id[Hash] = tk;
      tk = id[Tk] = Id;
      return 0;
    }
    else if (tk >= '0' && tk <= '9') {
      ival = tk - '0';
      while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0';
      tk = Num;
      return 0;
    }
    else if (tk == '/') {
      if (*p == '/') {
        ++p;
        while (*p != 0 && *p != '\n') ++p;
      }
      else {
        tk = Div;
        return 0;
      }
    }
    else if (tk == '\'' || tk == '"') {
      pp = data;
      while (*p != 0 && *p != tk) {
        if ((ival = *p++) == '\\') {
          if ((ival = *p++) == 'n') ival = '\n';
        }
        if (tk == '"') *data++ = ival;
      }
      ++p;
      if (tk == '"') ival = (int)pp; else tk = Num;
      return 0;
    }
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return 0; }
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return 0; }
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return 0; }
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return 0; }
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return 0; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return 0; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return 0; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return 0; }
    else if (tk == '^') { tk = Xor; return 0; }
    else if (tk == '%') { tk = Mod; return 0; }
    else if (tk == '*') { tk = Mul; return 0; }
    else if (tk == '[') { tk = Brak; return 0; }
    else if (tk == '?') { tk = Cond; return 0; }
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return 0;
  }
}

expr(int lev)
{
  int t, *d;

  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }
  else if (tk == '"') {
    *++e = LGA; *++e = ival; next();
    while (tk == '"') next();
    data = (char *)((int)data + 4 & -4); ty = PTR;
  }
  else if (tk == Id) {
    d = id; next();
    if (tk == '(') {
      next();
      t = 0;
      while (tk != ')') { expr(Assign); *++e = ARG; ++t; if (tk == ',') next(); }
      next();
      if (d[Class] == Sys) *++e = d[Val];
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
      else { printf("%d: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; }
      ty = d[Type];
    }
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
    else {
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }
      else if (d[Class] == Glo) { *++e = LGA; *++e = d[Val]; }
      else { printf("%d: undefined variable\n", line); exit(-1); }
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
    }
  }
  else if (tk == '(') {
    next();
    if (tk == Int || tk == Char) {
      t = (tk == Int) ? INT : CHAR; next();
      while (tk == Mul) { next(); t = t + PTR; }
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc);
      ty = t;
    }
    else {
      expr(Assign);
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  else if (tk == Mul) {
    next(); expr(Inc);
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
    *++e = (ty == CHAR) ? LC : LI;
  }
  else if (tk == And) {
    next(); expr(Inc);
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;
  }
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }
  else if (tk == Add) { next(); expr(Inc); ty = INT; }
  else if (tk == Sub) {
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) {
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; }
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? 4 : 1;
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;
  }
  else { printf("%d: bad expression\n", line); exit(-1); }

  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    t = ty;
    if (tk == Assign) {
      next();
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    else if (tk == Cond) {
      next();
      *++e = BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (int)(e + 1);
    }
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    else if (tk == Add) {
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = 4; *++e = MUL;  }
      *++e = ADD;
    }
    else if (tk == Sub) {
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = 4; *++e = MUL;  }
      *++e = SUB;
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    else if (tk == Inc || tk == Dec) {
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? 4 : 1;
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? 4 : 1;
      *++e = (tk == Inc) ? SUB : ADD;
      next();
    }
    else if (tk == Brak) {
      next(); *++e = PSH; expr(Assign);
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = 4; *++e = MUL;  }
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      *++e = ADD;
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}

stmt()
{
  int *a, *b;

  if (tk == If) {
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;
    stmt();
    if (tk == Else) {
      *b = (int)(e + 3); *++e = JMP; b = ++e;
      next();
      stmt();
    }
    *b = (int)(e + 1);
  }
  else if (tk == While) {
    next();
    a = e + 1;
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;
    stmt();
    *++e = JMP; *++e = (int)a;
    *b = (int)(e + 1);
  }
  else if (tk == Return) {
    next();
    if (tk != ';') expr(Assign);
    *++e = LEV;
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
  else if (tk == '{') {
    next();
    while (tk != '}') stmt();
    next();
  }
  else if (tk == ';') {
    next();
  }
  else {
    expr(Assign);
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

EmitZCode(int *pc, int *e, int *main)
{
	char *s;
	int i;
	int bStack;
	s=sdata;
	bStack=0;
	printf("Global a;\n");
	printf("Global d;\n");
	printf("Global s;\n");
	printf("Array memory -> 8191;\n");
	printf("Array locals -> 1024;\n");
	printf("Array data -> ");
	while (s<data)
	{
		printf("%d ", s[0]);
		s++;
	}
	printf("0 0;\n"); // make sure it's interpreted as an initialised Inform array
	printf("\n");
	printf("[main;\n");
	printf("\t@print \"C4 Compiled Code^^\";\n");
	printf("\t@add locals 1000 -> d;\n");
	printf("\t@storew d 0 0;\n"); // argv
	printf("\t@storew d 1 2;\n"); // argc
	printf("\t@call F%x d -> d;\n", main);
	printf("\t@quit;\n");
	printf("];\n\n");
	printf("[Malloc a;\n");
	printf("\t@store d s;\n");
	printf("\t@add s a -> s;\n");
	printf("\t@add memory d -> sp;\n");
	printf("\t@ret_popped;\n");
	printf("];\n\n");
	printf("[Memset bp dst val size;\n");
	printf("\t@loadw bp 0 -> size;\n");
	printf("\t@loadw bp 1 -> val;\n");
	printf("\t@loadw bp 2 -> dst;\n");
	printf(".memsetLoop;\n");
	printf("\t@jz size ?memsetReturn;\n");
	printf("\t @dec size;\n");
	printf("\t @storeb dst size val;\n");
	printf("\t jump memsetLoop;\n");
	printf(".memsetReturn;\n");
	printf("\t@nop;\n");
	printf("];\n\n");
	printf("[Memcmp bp src1 src2 size i;\n");
	printf("\t@loadw bp 0 -> size;\n");
	printf("\t@loadw bp 1 -> src1;\n");
	printf("\t@loadw bp 2 -> src2;\n");
	printf("\t@store bp 0;\n");
	printf(".memcmpLoop;\n");
	printf("\t@je i size ?memcmpReturn;\n");
	printf("\t @loadb src2 size -> sp;\n");
	printf("\t @loadb src1 size -> sp;\n");
	printf("\t @sub sp sp -> bp;\n");
	printf("\t @jz bp ?~memcmpReturn;\n");
	printf("\t @inc i;\n");
	printf("\t jump memcmpLoop;\n");
	printf(".memcmpReturn;\n");
	printf("\t@ret bp;\n");
	printf("];\n\n");
	printf("[PrintF bp numArgs c;\n");
	printf("\t@loadw bp numArgs -> d;\n");
	printf(".printFLoop;\n");
	printf("\t @loadb d 0 -> c;\n");
	printf("\t @jz c ?printArgs;\n");
	printf("\t  @print_char c;\n");
	printf("\t  @inc d;\n");
	printf("\tjump printFLoop;\n");
	printf(".printArgs;\n");
	printf("\t@jz numArgs ?printReturn;\n");
	printf("\t @dec numArgs;\n");
	printf("\t @loadw bp numArgs -> d;\n");
	printf("\t @print_char 32;\n");
	printf("\t @print_num d;\n");
	printf("\t jump printArgs;\n");
	printf(".printReturn;\n");
	printf("\t@new_line;\n");

	pc++;
	e++;
	while (pc!=e)
	{
		i = *pc++;
		if (i!=ENT)
			printf(".L%x;\n", pc-1);
		//printf("\t@print \"%x^\";\n", pc-1);
		
		if (i == LEA)
		{
			i=2*(*pc++);
			if (i<0)
				printf("\t@sub bp $%04x -> a;\n", -i);
			else
				printf("\t@add bp $%04x -> a;\n", i-4);
		}
		else if (i == LGA) printf("\t@add data $%04x -> a;\n", (*pc++)-(int)sdata);
		else if (i == IMM) printf("\t@store a $%04x;\n", *pc++);
		else if (i == JMP) printf("\tjump L%x;\n", *pc++);
		else if (i == JSR) printf("\t@call F%x argStack -> d;\n", *pc++);
		else if (i == BZ)  printf("\t@jz a ?L%x;\n", *pc++);
		else if (i == BNZ) printf("\t@jz a ?~L%x;\n", *pc++);
		else if (i == ENT) printf("];\n\n[F%x bp argStack;\n\t@sub bp $%04x -> argStack;\n", pc-1, 2*(*pc++));
		else if (i == ADJ) printf("\t@add argStack $%04x -> argStack;\n", 2*(*pc++));
		else if (i == LEV) printf("\t@rfalse;\n");
		else if (i == LI)  printf("\t@loadw a 0 -> a;\n");
		else if (i == LC)  printf("\t@loadb a 0 -> a;\n");
		else if (i == SI)  printf("\t@storew sp 0 a;\n");
		else if (i == SC)  printf("\t@storeb sp 0 a;\n");
		else if (i == PSH) printf("\t@push a;\n");
		else if (i == ARG) printf("\t@sub argStack 2 -> argStack;\n\t@storew argStack 0 a;\n");

		else if (i == OR)  printf("\t@or sp a -> a;\n");
		else if (i == AND) printf("\t@and sp a -> a;\n");
		else if (i == ADD) printf("\t@add sp a -> a;\n");
		else if (i == SUB) printf("\t@sub sp a -> a;\n");
		else if (i == MUL)  printf("\t@mul sp a -> a;\n");
		else if (i == DIV)  printf("\t@div sp a -> a;\n");
		else if (i == MOD)  printf("\t@mod sp a -> a;\n");
		else if (i == XOR)  printf("\t@store d sp;\n\t@or d a -> sp;\n\t@and d a -> sp;\n\t@not sp -> sp;\n\t@and sp sp -> a;\n");
		else if (i == EQ)   printf("\t@je sp a ?test1%x;\n\t@store a 0;\n\tjump test2%x;\n.test1%x;\n\t@store a 1;\n.test2%x;\n", pc, pc, pc, pc);
		else if (i == NE)   printf("\t@je sp a ?~test1%x;\n\t@store a 0;\n\tjump test2%x;\n.test1%x;\n\t@store a 1;\n.test2%x;\n", pc, pc, pc, pc);
		else if (i == LT)   printf("\t@jl sp a ?test1%x;\n\t@store a 0;\n\tjump test2%x;\n.test1%x;\n\t@store a 1;\n.test2%x;\n", pc, pc, pc, pc);
		else if (i == GE)   printf("\t@jl sp a ?~test1%x;\n\t@store a 0;\n\tjump test2%x;\n.test1%x;\n\t@store a 1;\n.test2%x;\n", pc, pc, pc, pc);
		else if (i == GT)   printf("\t@jg sp a ?test1%x;\n\t@store a 0;\n\tjump test2%x;\n.test1%x;\n\t@store a 1;\n.test2%x;\n", pc, pc, pc, pc);
		else if (i == LE)   printf("\t@jg sp a ?~test1%x;\n\t@store a 0;\n\tjump test2%x;\n.test1%x;\n\t@store a 1;\n.test2%x;\n", pc, pc, pc, pc);
		else if (i == SHL)  printf("\t@store d sp;\n\t@jz a ?test1%x;\n.test2%x;\n\t@mul d 2 -> d;\n\t@dec a;\n\t@jz a ?test2%x;\n\t.test1%x;\n\t@store a d;\n", pc, pc, pc, pc);
		else if (i == SHR)  printf("\t@store d sp;\n\t@jz a ?test1%x;\n.test2%x;\n\t@div d 2 -> d;\n\t@dec a;\n\t@jz a ?test2%x;\n\t.test1%x;\n\t@store a d;\n", pc, pc, pc, pc);
		
		else if (i == PRTF) printf("\t@call PrintF argStack $%04x -> a;\n", pc[1]-1);
		else if (i == MALC) printf("\t@call Malloc argStack -> a;\n");
		else if (i == MSET) printf("\t@call Memset argStack -> a;\n");
		else if (i == MCMP) printf("\t@call Memcmp argStack -> a;\n");
		else if (i == OPEN) printf("\t@store a $ffff;\n");
		else if (i == READ) printf("\t@store a 0;\n");
		else if (i == CLOS) printf("\t@store a 0;\n");
		else if (i == EXIT) printf("\t@quit;\n");
		else printf("unknown instruction = %d!\n", i);
	}
	printf("];\n");
}

main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain;
  int *pc, *sp, *bp, a, cycle; // vm registers
  int i, *t; // temps

  --argc; ++argv;
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'z') { zcode = 1; --argc; ++argv; }
  if (argc < 1) { printf("usage: c4 [-s] [-d] [-z] file ...\n"); return -1; }

  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

  poolsz = 256*1024; // arbitrary size
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(se = le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(sdata = data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  p = "char else enum if int return while "
      "open read close printf malloc memset memcmp exit main";
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
  next(); idmain = id; // keep track of main

  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0;
  close(fd);

  // parse declarations
  line = 1;
  next();
  while (tk) {
    bt = INT; // basetype
    if (tk == Int) next();
    else if (tk == Char) { next(); bt = CHAR; }
    else if (tk == Enum) {
      next();
      if (tk != '{') next();
      if (tk == '{') {
        next();
        i = 0;
        while (tk != '}') {
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          if (tk == Assign) {
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++;
          if (tk == ',') next();
        }
        next();
      }
    }
    while (tk != ';' && tk != '}') {
      ty = bt;
      while (tk == Mul) { next(); ty = ty + PTR; }
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
      next();
      id[Type] = ty;
      if (tk == '(') { // function
        id[Class] = Fun;
        id[Val] = (int)(e + 1);
        next(); i = 0;
        while (tk != ')') {
          ty = INT;
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;
          next();
          if (tk == ',') next();
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        loc = ++i;
        next();
        while (tk == Int || tk == Char) {
          bt = (tk == Int) ? INT : CHAR;
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;
            next();
            if (tk == ',') next();
          }
          next();
        }
        *++e = ENT; *++e = i - loc;
        while (tk != '}') stmt();
        *++e = LEV;
        id = sym; // unwind symbol table locals
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else {
        id[Class] = Glo;
        id[Val] = (int)data;
        data = data + 4;
      }
      if (tk == ',') next();
    }
    next();
  }

  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }
  if (zcode) { EmitZCode(se, e, pc); return 0; }
  if (src) return 0;

  // setup stack
  sp = (int *)((int)sp + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = argc;
  *--sp = (int)argv;
  *--sp = (int)t;

  // run...
  cycle = 0;
  while (1) {
    i = *pc++; ++cycle;
    if (debug) {
      printf("%d> %.4s", cycle,
        &"LEA ,LGA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,ARG ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }

    if      (i == LEA) a = (int)(bp + *pc++);                             // load local address
    else if (i == IMM || i==LGA) a = *pc++;                                         // load global address or immediate
    else if (i == JMP) pc = (int *)(*pc);                                 // jump
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
    else if (i == LI)  a = *(int *)a;                                     // load int
    else if (i == LC)  a = *(char *)a;                                    // load char
    else if (i == SI)  *(int *)*sp++ = a;                                 // store int
    else if (i == SC)  a = *(char *)*sp++ = a;                            // store char
    else if (i == PSH || i == ARG) *--sp = a;                             // push

    else if (i == OR)  a = *sp++ |  a;
    else if (i == XOR) a = *sp++ ^  a;
    else if (i == AND) a = *sp++ &  a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)  a = *sp++ != a;
    else if (i == LT)  a = *sp++ <  a;
    else if (i == GT)  a = *sp++ >  a;
    else if (i == LE)  a = *sp++ <= a;
    else if (i == GE)  a = *sp++ >= a;
    else if (i == SHL) a = *sp++ << a;
    else if (i == SHR) a = *sp++ >> a;
    else if (i == ADD) a = *sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ *  a;
    else if (i == DIV) a = *sp++ /  a;
    else if (i == MOD) a = *sp++ %  a;

    else if (i == OPEN) a = open((char *)sp[1], *sp);
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);
    else if (i == CLOS) a = close(*sp);
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
    else if (i == MALC) a = (int)malloc(*sp);
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
  }
}
