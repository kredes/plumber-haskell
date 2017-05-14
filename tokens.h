#ifndef tokens_h
#define tokens_h
/* tokens.h -- List of labelled tokens and stuff
 *
 * Generated from: plumber.g
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * ANTLR Version 1.33MR33
 */
#define zzEOF_TOKEN 1
#define INPUT 2
#define PRINT 3
#define DRAW 4
#define OPENP 5
#define CLOSEP 6
#define WHILE 7
#define IF 8
#define ELSE 9
#define MERGE 10
#define SPLIT 11
#define TUBE 12
#define PUSH 13
#define POP 14
#define FULL 15
#define EMPTY 16
#define TUBEVECT 17
#define OF 18
#define CONN 19
#define LENGTH 20
#define DIAMETER 21
#define NUM 22
#define DNUM 23
#define LESS 24
#define GREAT 25
#define ASSIG 26
#define EQUAL 27
#define AND 28
#define OR 29
#define NOT 30
#define PLUS 31
#define MINUS 32
#define TIMES 33
#define SC 34
#define ENDWHILE 35
#define ENDIF 36
#define ID 37
#define SPACE 38

#ifdef __USE_PROTOS
void plumber(AST**_root);
#else
extern void plumber();
#endif

#ifdef __USE_PROTOS
void ops(AST**_root);
#else
extern void ops();
#endif

#ifdef __USE_PROTOS
void input(AST**_root);
#else
extern void input();
#endif

#ifdef __USE_PROTOS
void print(AST**_root);
#else
extern void print();
#endif

#ifdef __USE_PROTOS
void draw(AST**_root);
#else
extern void draw();
#endif

#ifdef __USE_PROTOS
void length(AST**_root);
#else
extern void length();
#endif

#ifdef __USE_PROTOS
void diameter(AST**_root);
#else
extern void diameter();
#endif

#ifdef __USE_PROTOS
void conditional(AST**_root);
#else
extern void conditional();
#endif

#ifdef __USE_PROTOS
void iter(AST**_root);
#else
extern void iter();
#endif

#ifdef __USE_PROTOS
void assign(AST**_root);
#else
extern void assign();
#endif

#ifdef __USE_PROTOS
void connect(AST**_root);
#else
extern void connect();
#endif

#ifdef __USE_PROTOS
void merge(AST**_root);
#else
extern void merge();
#endif

#ifdef __USE_PROTOS
void base(AST**_root);
#else
extern void base();
#endif

#ifdef __USE_PROTOS
void catom(AST**_root);
#else
extern void catom();
#endif

#ifdef __USE_PROTOS
void tatom(AST**_root);
#else
extern void tatom();
#endif

#ifdef __USE_PROTOS
void split(AST**_root);
#else
extern void split();
#endif

#ifdef __USE_PROTOS
void literal(AST**_root);
#else
extern void literal();
#endif

#ifdef __USE_PROTOS
void vect(AST**_root);
#else
extern void vect();
#endif

#ifdef __USE_PROTOS
void pushpop(AST**_root);
#else
extern void pushpop();
#endif

#ifdef __USE_PROTOS
void cond(AST**_root);
#else
extern void cond();
#endif

#ifdef __USE_PROTOS
void cond1(AST**_root);
#else
extern void cond1();
#endif

#ifdef __USE_PROTOS
void cond2(AST**_root);
#else
extern void cond2();
#endif

#ifdef __USE_PROTOS
void cond3(AST**_root);
#else
extern void cond3();
#endif

#ifdef __USE_PROTOS
void boolFunc(AST**_root);
#else
extern void boolFunc();
#endif

#ifdef __USE_PROTOS
void expr(AST**_root);
#else
extern void expr();
#endif

#ifdef __USE_PROTOS
void term(AST**_root);
#else
extern void term();
#endif

#ifdef __USE_PROTOS
void val(AST**_root);
#else
extern void val();
#endif

#endif
extern SetWordType zzerr1[];
extern SetWordType zzerr2[];
extern SetWordType setwd1[];
extern SetWordType zzerr3[];
extern SetWordType zzerr4[];
extern SetWordType zzerr5[];
extern SetWordType setwd2[];
extern SetWordType zzerr6[];
extern SetWordType zzerr7[];
extern SetWordType zzerr8[];
extern SetWordType zzerr9[];
extern SetWordType setwd3[];
extern SetWordType zzerr10[];
extern SetWordType zzerr11[];
extern SetWordType zzerr12[];
extern SetWordType zzerr13[];
extern SetWordType setwd4[];
extern SetWordType zzerr14[];
extern SetWordType zzerr15[];
extern SetWordType setwd5[];
