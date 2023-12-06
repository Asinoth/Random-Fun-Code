#include "SymbolTableHeader.h"

bool executionFinished=false;
unsigned pc=0;
unsigned currLine=0;
unsigned codeSize=0;
unsigned totalNumConsts;
unsigned totalStringConsts;
unsigned totalNamedLibfuncs;
unsigned totalUserFuncs;
std::vector<double> numConsts;
std::vector<char *> stringConsts;
std::vector<char *> namedLibfuncs;
std::vector<struct userfunc *> userFuncs;
std::stack<struct userfunc *> funcstack;

typedef double (*arithmetic_func_t)(double d, double y);
double add_impl(double x, double y) { return x + y; }
double sub_impl(double x, double y) { return x - y; }
double mul_impl(double x, double y) { return x * y; }
double div_impl(double x, double y) { return x / y; /*error checking?*/ }
double mod_impl(double x, double y) { return ((unsigned)x) % ((unsigned)y); /*error checking?*/ }

arithmetic_func_t arithmeticFuncs[] = {
    add_impl,
    sub_impl,
    mul_impl,
    div_impl,
    mod_impl};

enum vmopcode
{
    assign_v,
    add_v,
    sub_v,
    mul_v,
    div_v,
    mod_v,
    uminus_v,
    and_v,
    cr_v,
    not_v,
    jeq_v,
    jne_v,
    jle_v,
    jge_v,
    jlt_v,
    jgt_v,
    call_v,
    pusharg_v,
    funcenter_v,
    funcexit_v,
    newtable_v,
    tablegetelem_v,
    tablesetelem_v,
    nop_v,
    jump_v
};

typedef enum vmarg_t
{
    label_a = 0,
    global_a = 1,
    formal_a = 2,
    local_a = 3,
    number_a = 4,
    string_a = 5,
    bool_a = 6,
    nil_a = 7,
    userfunc_a = 8,
    libfunc_a = 9,
    retval_a = 10
} vmarg_t;

typedef struct vmarg
{
    enum vmarg_t type;
    unsigned val;
} vmarg;

struct userfunc
{
    unsigned address;
    unsigned localSize;
    char *id;

};

unsigned consts_newstring(char *s)
{
    for (std::vector<char *>::iterator i = stringConsts.begin(); i != stringConsts.end(); ++i)
    {
        if (strcmp(*i, s) == 0)
        {
            int p = std::distance(stringConsts.begin(), i);
            return p;
        }
    }
    stringConsts.push_back(s);
    return totalStringConsts++;
}

unsigned consts_newnumber(double n)
{
    for (std::vector<double>::iterator i = numConsts.begin(); i != numConsts.end(); ++i)
    {
        if (*i == n)
        {
            int p = std::distance(numConsts.begin(), i);
            return p;
        }
    }
    numConsts.push_back(n);
    return totalNumConsts++;
}

unsigned libfuncs_newused(char *s)
{
    for (std::vector<char *>::iterator i = namedLibfuncs.begin(); i != namedLibfuncs.end(); ++i)
    {
        if (strcmp(*i, s) == 0)
        {
            int p = std::distance(namedLibfuncs.begin(), i);
            return p;
        }
    }
    namedLibfuncs.push_back(s);
    return totalNamedLibfuncs++;
}

unsigned userfuncs_newfunc(SymbolTableEntry *s)
{
    for (std::vector<struct userfunc *>::iterator i = userFuncs.begin(); i != userFuncs.end(); ++i)
    {
        if (strcmp((*i)->id, s->name) == 0)
        {
            int p = std::distance(userFuncs.begin(), i);
            return p;
        }
    }
    userfunc *my_userfunc = (struct userfunc *)malloc(sizeof(struct userfunc));
    my_userfunc->id = s->name;
    my_userfunc->address = s->iaddress;
    my_userfunc->localSize = s->totalcalls;
    userFuncs.push_back(my_userfunc);
    return totalUserFuncs++;
}

struct incomplete_jump
{
    unsigned instrNo;      //jump instruction mumber
    unsigned iaddress;     //i-code jump-target address
    incomplete_jump *next; //trivial linked list
} incomplete_jump;

struct incomplete_jump *ij_head = (struct incomplete_jump *)0;
unsigned ij_total = 0;
struct instruction *code = (instruction *)0;

void add_incomplete_jump(unsigned instrNo, unsigned iaddress)
{
   struct incomplete_jump *j = (struct incomplete_jump*)malloc(sizeof(incomplete_jump));
   j->instrNo = instrNo;
   j->iaddress = iaddress;
   j->next = NULL;
   ij_head = j;
   ij_total++;

}


/*void patch_incomplete_jumps() {
for each incomplete jump x {
	if x.iaddress = intermediate code size then
		instructions[x.instrN o].result = target code size;
	else
		instructions[x.instrN o].result = quads[x.iaddress].taddress;
}*/

typedef enum avm_memcell_t
{
    number_m = 0,
    bool_m = 2,
    string_m = 1,
    table_m = 3,
    userfunc_m = 4,
    libfunc_m = 5,
    nil_m = 6,
    undef_m = 7
} type;

struct avm_table;

typedef struct avm_memcell
{
    enum avm_memcell_t type;
    union
    {
        double numVal;
        char *strVal;
        unsigned char boolVal;
        struct avm_table *tableVal;
        unsigned funcVal;
        char *libfuncVal;
    } data;

} avm_memcell;

struct avm_table_bucket
{
    struct avm_memcell *key;
    struct avm_memcell *value;
    struct avm_table_bucket *next;
};

avm_memcell ax, bx, cx;
avm_memcell retval;
unsigned top, topsp;

double consts_getnumber(unsigned index)
{
}

char *consts_getstring(unsigned index)
{
}

char *libfuncs_getused(unsigned index)
{
}

/*
For simplicity, we only show support for numeric and string keys.
Bonus for teams implementing keys for user functions, libraryfunctions, and booleans.
*/
struct avm_table
{
    unsigned refCounter;
    struct avm_table_bucket *strIndexed[AVM_TABLE_HASHSIZE];
    struct avm_table_bucket *numIndexed[AVM_TABLE_HASHSIZE];
    unsigned total;
};

avm_memcell avm_stack[AVM_STACKSIZE];
static void avm_initstack()
{
    for (unsigned i = 0; i < AVM_STACKSIZE; ++i)
    {
        AVM_WIPEOUT(avm_stack[i]);
        avm_stack[i].type = undef_m;
    }
}

typedef unsigned char (*tobool_func_t)(avm_memcell *);

unsigned char number_tobool(avm_memcell *m) { return m->data.numVal != 0; }
unsigned char string_tobool(avm_memcell *m) { return m->data.strVal[0] != 0; }
unsigned char bool_tobool(avm_memcell *m) { return m->data.boolVal; }
unsigned char table_tobool(avm_memcell *m) { return 1; }
unsigned char userfunc_tobool(avm_memcell *m) { return 1; }
unsigned char libfunc_tobool(avm_memcell *m) { return 1; }
unsigned char nil_tobool(avm_memcell *m) { return 0; }
unsigned char undef_tobool(avm_memcell *m)
{
    assert(0);
    return 0;
}

tobool_func_t toboolFuncs[] = {
    number_tobool,
    string_tobool,
    bool_tobool,
    table_tobool,
    userfunc_tobool,
    libfunc_tobool,
    nil_tobool,
    undef_tobool};

unsigned char avm_tobool(avm_memcell *m)
{
    assert(m->type >= 0 && m->type < undef_m);
    return (*toboolFuncs[m->type])(m);
}

std::string typeStrings[] = {
    "number",
    "string",
    "bool",
    "table",
    "userfunc",
    "libfunc",
    "nil",
    "undef"};

struct instruction
{
    enum vmopcode vmop;
    struct vmarg result;
    struct vmarg arg1;
    struct vmarg arg2;
    unsigned srcLine;
};

typedef void (*generator_func_t)(quad *);

void avm_tableincrefcounter(avm_table *t) { ++t->refCounter; }

void avm_tabledestroy(avm_table *t);

//automatic garbage collection for tables when reference counter gets zero
void avm_tabledecrefcounter(avm_table *t)
{
    assert(t->refCounter > 0);
    if (!(--t->refCounter))
        avm_tabledestroy(t);
}

void avm_tablebucketsinit(avm_table_bucket **p)
{
    for (unsigned i = 0; i < AVM_TABLE_HASHSIZE; ++i)
        p[i] = (avm_table_bucket *)0;
}

avm_table *avm_tablenew()
{
    avm_table *t = (avm_table *)malloc(sizeof(avm_table));
    AVM_WIPEOUT(*t);

    t->refCounter = t->total = 0;
    avm_tablebucketsinit(t->numIndexed);
    avm_tablebucketsinit(t->strIndexed);

    return t;
}

typedef void (*memclear_func_t)(avm_memcell *);

void memclear_string(avm_memcell *m)
{
    assert(m->data.strVal);
    free(m->data.strVal);
}

void memclear_table(avm_memcell *m)
{
    assert(m->data.strVal);
    avm_tabledecrefcounter(m->data.tableVal);
}

memclear_func_t memclearFuncs[] = {
    0, //number
    memclear_string,
    0, //bool
    memclear_table,
    0, //userfunc
    0, //libfunc
    0, //nil
    0  //undef
};

void avm_memcellclear(struct avm_memcell *m)
{
    if (m->type != undef_m)
    {
        memclear_func_t f = memclearFuncs[m->type];
        if (f)
            (*f)(m);
        m->type = undef_m;
    }
}

void avm_tablebucketsdestroy(avm_table_bucket **p)
{

    for (unsigned i = 0; i < AVM_TABLE_HASHSIZE; ++i, ++p)
    {
        for (avm_table_bucket *b = *p; b;)
        {
            avm_table_bucket *del = b;
            b = b->next;
            avm_memcellclear(del->key);   //&del->key stin dialeksi
            avm_memcellclear(del->value); //&del->value stin dialeksi
            free(del);
        }
        p[i] = (avm_table_bucket *)0;
    }
}

void avm_tabledestroy(avm_table *t)
{
    avm_tablebucketsdestroy(t->strIndexed);
    avm_tablebucketsdestroy(t->numIndexed);
    free(t);
}

typedef bool (*cmp_func)(double, double);

void make_operand(expr *e, vmarg *arg)
{
    switch (e->type)
    {
    case var_e:
    case tableitem_e:
    case arithexpr_e:
    case boolexpr_e:
    case newtable_e:
    {
        assert(e->entry);
        arg->val = e->entry->offset;

        switch (e->entry->space)
        {

        case programvar:
            arg->type = global_a;
            break;
        case functionlocal:
            arg->type - local_a;
            break;
        case formalarg:
            arg->type = formal_a;
            break;
        }
        break;
    }
    case constbool_e:
    {
        arg->val = e->boolConst;
        arg->type = bool_a;
        break;
    }
    case conststring_e:
    {
        arg->val = consts_newstring(e->strConst);
        arg->type = string_a;
        break;
    }
    case constnum_e:
    {
        arg->val = consts_newnumber(e->numConst);
        arg->type = number_a;
        break;
    }
    case nil_e:
        arg->type = nil_a;
        break;

    case programfunc_e:
    {
        arg->type = userfunc_a;
        arg->val = e->entry->iaddress; //taddress originally
        break;
    }
    case libraryfunc_e:
    {
        arg->type = libfunc_a;
        arg->val = libfuncs_newused(e->entry->name);
        break;
    }
    }
}

//Helper functions to produce common arguments for
//generated instructions, like 1, 0, "true", "false"
//and function return value
void make_numberoperand(vmarg *arg, double val)
{
    arg->val = consts_newnumber(val);
    arg->type = number_a;
}

void make_booloperand(vmarg *arg, unsigned val)
{
    arg->val = val;
    arg->type = bool_a;
}

void make_retvaloperand(vmarg *arg)
{
    arg->type = retval_a;
}

void execute_cycle()
{
    /*if(executionFinished)
        break;
else*/
    if (pc == AVM_ENDING_PC)
    {
        executionFinished = true;
        //break; //return exei ston pseudocode
    }
    else
    {
        assert(pc < AVM_ENDING_PC);
        instruction *instr = code + pc;
        //assert(instr->vmopcode >=0 && instr->vmopcode <=AVM_MAX_INSTRUCTIONS); //opcode originally
        if (instr->srcLine)
            currLine = instr->srcLine;
        unsigned oldPC = pc;
        //(*executeFuncs[instr->vmopcode])(instr); //opcode originally
        if (pc == oldPC)
            ++pc;
    }
}

std::vector<struct instruction> instrVec;

int nextinstructionlabel()
{
    return instrVec.size();
}

void emit_v(instruction t)
{
    instrVec.push_back(t);
}

void generate(vmopcode vop, quad *q)
{
    instruction t;
    t.vmop = vop;
    make_operand(q->arg1, &t.arg1);
    make_operand(q->arg2, &t.arg2);
    make_operand(q->result, &t.result);
    q->taddress = nextinstructionlabel();
    emit_v(t);
}

void generate_ADD(quad *q) { generate(add_v, q); }
void generate_SUB(quad *q) { generate(sub_v, q); }
void generate_MUL(quad *q) { generate(mul_v, q); }
void generate_DIV(quad *q) { generate(div_v, q); }
void generate_MOD(quad *q) { generate(mod_v, q); }

void generate_relational(vmopcode vmop, quad *q)
{
    instruction t;
    t.vmop = vmop;
    make_operand(q->arg1, &t.arg1);
    make_operand(q->arg2, &t.arg2);
    t.result.type = label_a;
    if (q->label < currQuad)
        t.result.val = quads[q->label].taddress;
    else
        add_incomplete_jump(nextinstructionlabel(), q->label);
    q->taddress = nextinstructionlabel();
    emit_v(t);
}

void generate_NEWTABLE(quad *q) { generate(newtable_v, q); }
void generate_TABLEGETELM(quad *q) { generate(tablegetelem_v, q); }
void generate_TABLESETELEM(quad *q) { generate(tablesetelem_v, q); }
void generate_ASSIGN(quad *q) { generate(assign_v, q); }
void generate_NOP(quad *q)
{ /*instruction t; t.opcode=nop; emit(t); */
}
void generate_JUMP(quad *q) { generate_relational(jump_v, q); }
void generate_IF_EQ(quad *q) { generate_relational(jeq_v, q); }
void generate_IF_NOTEQ(quad *q) { generate_relational(jne_v, q); }
void generate_IF_GREATER(quad *q) { generate_relational(jgt_v, q); }
void generate_IF_GREATEREQ(quad *q) { generate_relational(jge_v, q); }
void generate_IF_LESS(quad *q) { generate_relational(jlt_v, q); }
void generate_IF_LESSEQ(quad *q) { generate_relational(jle_v, q); }
void generate_NOT(quad *q)
{
    q->taddress = nextinstructionlabel();
    instruction t;
    t.vmop = jeq_v;
    make_operand(q->arg1, &t.arg1);
    make_booloperand(&t.arg2, false);
    t.result.type = label_a;
    t.result.val = nextinstructionlabel()+3;
    emit_v(t);
    t.vmop = assign_v;
    make_booloperand(&t.arg1, false);
    reset_operand(&t.arg2);
    make_operand(q->result, &t.result);
    emit_v(t);
    //... more
    t.vmop = jump_v;
    reset_operand(&t.arg1);
    reset_operand(&t.arg2);
    t.result.type = label_a;
    t.result.val = nextinstructionlabel()+2;
    emit_v(t);
    t.vmop = assign_v;
    make_booloperand(&t.arg1, true);
    reset_operand(&t.arg2);
    make_operand(q->result, &t.result);
    emit_v(t);
}

void generate_OR(quad *q)
{
    q->taddress = nextinstructionlabel();
    instruction t;
    t.vmop = jeq_v;
    make_operand(q->arg1, &t.arg1);
    make_booloperand(&t.arg2, true);
    t.result.type = label_a;
    t.result.val = nextinstructionlabel()+4;
    emit_v(t);
    make_operand(q->arg2, &t.arg1);
    t.result.val = nextinstructionlabel()+3;
    emit_v(t);
    t.vmop = assign_v;
    make_booloperand(&t.arg1, false);
    reset_operand(&t.arg2);
    make_operand(q->result, &t.result);
    emit_v(t);
    t.vmop = jump_v;
    reset_operand (&t.arg1);
    reset_operand(&t.arg2);
    t.result.type = label_a;
    t.result.val = nextinstructionlabel()+2;
    emit_v(t);
    t.vmop = assign_v;
    make_booloperand(&t.arg1, true);
    reset_operand(&t.arg2);
    make_operand(q->result, &t.result);
    emit_v(t);
}

void generate_PARAM(quad *q)
{
    q->taddress = nextinstructionlabel();
    instruction t;
    t.vmop = pusharg_v;
    make_operand(q->arg1, &t.arg1);
    emit_v(t);
}

void generate_CALL(quad *q)
{
    q->taddress = nextinstructionlabel();
    instruction t;
    t.vmop = call_v;
    make_operand(q->arg1, &t.arg1);
    emit_v(t);
}

void generate_GETRETVAL(quad *q)
{
    q->taddress = nextinstructionlabel();
    instruction t;
    t.vmop = assign_v;
    make_operand(q->result, &t.result);
    make_retvaloperand(&t.arg1);
    emit_v(t);
}

void generate_FUNCSTART(quad *q) {
    userfunc *f = (struct userfunc*)malloc(sizeof(userfunc));
    instruction t;
    t.vmop = funcenter_v;
    f->address = nextinstructionlabel();
    q->taddress = nextinstructionlabel();
    f->localSize = q->result->entry->totalcalls;
    f->id = q->result->entry->name;
    userFuncs.push_back(f);
    funcstack.push(f);
    t.result.type = label_a;
    make_operand(q->result, &t.result);
    emit_v(t);
}

void generate_FUNCEND(quad *q) {
    userfunc* f = funcstack.top();
    funcstack.pop();
    backpatch(f,nextinstructionlabel());
    q->taddress = nextinstructionlabel();
    instruction t; 
    t.vmop = funcexit_v;
    make_operand(q->result, &t.result);
    emit_v(t);
}

void generate_TABLEGETELEM(quad *q ) {
    generate(tablegetelem_v, q);
}

void generate_UMINUS(quad *q) {}

void generate_AND(quad *q) {

}
void generate_RETURN(quad *q) {
    instruction t;
    q->taddress = nextinstructionlabel();
    t.vmop = assign_v; //??????????????? NO IDEA
    make_retvaloperand(&t.result);
    make_operand(q->arg1,&t.arg1);
    emit_v(t);
    userfunc *f = funcstack.top();
    funcstack.pop();
    f.returnList.push_back(nextinstructionlabel());
    t.vmop = jump_v;
    reset_operand(&t.arg1);
    reset_operand(&t.arg2);
    t.result.type = label_a;
    emit_v(t);
}

void generate_TABLECREATE(quad *q) {
    generate(newtable_v, q);
}

void execute_assign(instruction *i) {}
void execute_add(instruction *i) {}
void execute_sub(instruction *i) {}
void execute_mul(instruction *i) {}
void execute_div(instruction *i) {}
void execute_mod(instruction *i) {}
void execute_uminus(instruction *i) {}
void execute_and(instruction *i) {}
void execute_or(instruction *i) {}
void execute_not(instruction *i) {}
void execute_jeq(instruction *i) {}
void execute_jne(instruction *i) {}
void execute_jle(instruction *i) {}
void execute_jge(instruction *i) {}
void execute_jlt(instruction *i) {}
void execute_jgt(instruction *i) {}
void execute_call(instruction *i) {}
void execute_pusharg(instruction *i) {}
void execute_funcenter(instruction *i) {}
void execute_funcexit(instruction *i) {}
void execute_newtable(instruction *i) {}
void execute_tablegetelem(instruction *i) {}
void execute_tablesetelem(instruction *i) {}
void execute_nop(instruction *i) {}

typedef void (*execute_func_t)(instruction *); //den kserw an einai swsto ayto edw
execute_func_t executeFuncs[24] = {
    execute_assign,
    execute_add,
    execute_sub,
    execute_mul,
    execute_div,
    execute_mod,
    execute_uminus,
    execute_and,
    execute_or,
    execute_not,
    execute_jeq,
    execute_jne,
    execute_jle,
    execute_jge,
    execute_jlt,
    execute_jgt,
    execute_call,
    execute_pusharg,
    execute_funcenter,
    execute_funcexit,
    execute_newtable,
    execute_tablegetelem,
    execute_tablesetelem,
    execute_nop};

generator_func_t generatorp[26] = {
    generate_ASSIGN,
    generate_ADD,
    generate_SUB,
    generate_MUL,
    generate_DIV,
    generate_MOD,
    generate_UMINUS,
    generate_AND,
    generate_OR,
    generate_NOT,
    generate_IF_EQ,
    generate_IF_NOTEQ,
    generate_IF_LESSEQ,
    generate_IF_GREATEREQ,
    generate_IF_LESS,
    generate_IF_GREATER,
    generate_CALL,
    generate_PARAM,
    generate_RETURN,
    generate_GETRETVAL,
    generate_FUNCSTART,
    generate_FUNCEND,
    generate_TABLECREATE,
    generate_TABLESETELEM,
    generate_TABLEGETELEM,
    generate_JUMP};

void generate2(void)
{

    for (unsigned i = 0; i < currQuad; ++i)
        (*generatorp[quads[i].op])(quads + i);
}

/*
void execute_tablegetelem(instruction* instr) {

avm_memcell* lv=avm_translate_operand(&instr->result, (avm_memcell*)0);
avm_memcell* t=avm_translate_operand(&instr->arg1,(avm_memcell*)0);
avm_memcell* i=avm_translate_operand(&instr->arg2,&ax);

assert(lv&&(&stack[0]<=lv &&&stack[top]>lv || lv==&retval));
assert(t&&&stack[0]<=t&&&stack[top]>t);
assert(i);

avm_memcellclear(lv);
lv->type=nil_m;
if(t->type!=table_m)
        avm_error("Illegal use of type %s as table!",typeStrings[t->type]);
else {
        avm_memcell* content=avm_tablegetelem(t->data.tableVal, i);
        if(content) avm_assign(lv, content);
        else {
                char* ts=avm_tostring(t);
                char* is=avm_tostring(i);
                avm_warning("%s[%s] not found!",ts, is);
                free(ts);
                free(is);
        }

}

}


void execute_tablesetelem(instruction* instr) {
avm_memcell* t=avm_translate_operand(&instr->result, (avm_memcell*)0);
avm_memcell* i=avm_translate_operand(&instr->arg1, &ax);
avm_memcell* c=avm_translate_operand(&instr->arg2, &bx);

assert(t&&&stack[0]<=t&&&stack[top]>t); //why not check got retval registers?
assert(i);

if(t->type!=table_m)
	avm_error("Illegal use of type %s as table!",typeStrings[t->type]);
else
	avm_tablesetelem(t->Data.tableVal, i, c);
}


void execute_newtable(instruction* instr) {
	avm_memcell* lv=avm_translate_operand(&instr->result, (avm_memcell*)0);
	assert(lv && (&stack[0]<=lv && &stack[top]>lv || lv==&retval));

	avm_memcellclear(lv);
	lv->type=table_m;
	lv->data.tableVal=avm_tablenew();
	avm_tableincrefcounter(lv->data.tableVal);

}



void execute_jeq(instruction* instr) {

assert(instr->result.type==label_a);
avm_memcell* rv1=avm_translate_operand(&instr->arg1, &ax);
avm_memcell* rv2=avm_translate_operand(&instr->arg2, &bx);

unsigned char result=0;

if(rv1->type==undef_m || rv2->type==undef_m)
	avm_error("'undef' involved in equality!");
else if(rv1->type==nil_m || rv2->type=nil_m)
	result=rv1->type==nil_m && rv2->type==nil_m;
else if(rv1->type==bool_m || rv2->type=bool_m)
        result=(avm_tobool(rv1)->type==avm_tobool(rv2));
else if(rv1->type!=rv2->type)
        avm_error("%s==%s is illegal!");
	typeStrings[rv1->type],typeStrings[rv2->type]);
else //equality check with dispatching. Arkei na kanw dispatch ws pros ton typo toy rv1
{break; }

if(!executionFinished&&result)
	pc=instr->result.val;
}


void execute_arithmetic(instruction* instr) {

avm_memcell* lv=avm_translate_operand(&instr->result, (avm_memcell*) 0);
avm_memcell* rv1=avm_translate_operand(&instr->arg1, &ax);
avm_memcell* rv2=avm_translate_operand(&instr->arg2, &bx);

assert(lv && (&stack[0]<=lv && &stack[top]>lv || lv==&retval));
assert(rv1&&rv2);

if(rv1->type!=number_m || rv2->type!=number_m){
avm_error("Not a number in arithmetic!");
executionFinished=true;
}
else {
	arithmetic_func_t op=arithmeticFuncs[instr->opcode-add_v];
	avm_memcellclear(lv);
	lv->type=number_m;
	lv->data.numVal=(*op)(rv1->data.numVal, rv2->data.numVal);
}


}

}



unsigned avm_totalactuals() {
return avm_get_envvalue(topsp+AVM_NUMACTUALS_OFFSET);
}

avm_memcell* avm_getactual(unsigned i) {
assert(i<avm_totalactuals());
return %stack[topsp+AVM_STACKENV_SIZE+1+i];
}

//implementation of the library function 'print'
void libfunc_print() {
unsigned n=avm_totalactuals();
for(unsigned i=0;i<n;++i) {
	char* s=avm_tostring(avm_getactual(i));
	puts(s);
	free(s);
}

}

//With the following, every library function is manually added in the VM library function resolution map.
void avm_registerlibfunc(char* id,library_func_t addr)
{
	
}


void libfunc_typeof(void) {
	unsigned n=avm_totalactuals();
	if(n!=1) avm_error("One argument (not %d) expected in 'typeof'!",n);
	else {
		//that's how a lib func returns a result
		//it has to just set teh retval register!
		avm_memcellclear(&retval);
		retval.type=string_m;
		retval.data.strVal=strdup(typeStrings[avm_getactual(0)->type]);
	}

}

void libfunc_totalarguments() {
//get topsp of previous activation record

unsigned p_topsp=avm_get+envvalue(topsp+AVM_SAVEDTOPSP_OFFSET);
if(!p_topsp) { //If 0, no previous activation record
avm_error("'totalarguments' called outside a function!");
retval.type=nil_m;
}
else {
//extract the number of actual arguments for the previous activation record
retval.type=number_m;
retval.data.numval=avm_get_envvalue(p_topsp+AVM_NUMACTUALS_OFFSET);
}

}



//Synartisi metatropis vmargs se avm_memcell*
avm_memcell* avm_translate_operand(vmarg* arg, avm_memcell* reg)
{

switch(arg->type) {

	//variables
	case global_a: return &stack[AVM_STACKSIZE-1-arg->val];
	case local_a: return &stack[topsp-arg->val];
	case formal_a: return &stack[topsp+AVM_STACKENV_SIZE+1+arg->val];
	case retval_a: &retval;

	case number_a: {
		reg->type=number_m;
		reg->data.numVal=consts_getnumber(arg->val);
		return reg;
	}
	case string_a: {
		reg->type=string_m;
		reg->data.strVal=consts_getstring(arg->val);
		return reg;
	}
	case bool_a: {
                reg->type=bool_m;
                reg->data.boolVal=arg->val;
                return reg;
        }
	case nil_a: reg->type=nil_m; return reg;
	
	case userfunc_a:  {
		reg->type=userfunc_m;
                reg->data.funcVal=arg->val;
                return reg;
	}
	case libfunc_a: {
		reg->type=libfunc_m;
                reg->data.libfuncVal=libfuncs_getused(arg->val);
                return reg;
	}

  }

}

void avm_calllibfunc(char* id) {
libraryfunc_t f=avm_getlibraryfunc(id);
if(!f) {
        avm_error("Unsupported lib func '%s' called!", id);
        executionFinished=true;
}
else {

//Notice that enter/exit functions are called manually
topsp=top; //enter function sequence. No stack locals.
totalActuals=0;
(*f)(); //call lib func
if(!executionFinished) //error may naturally occur inside
        execute_funcexit((instruction*)0);

}

}


//call, swsimo perivallontos kai klhsh
void avm_callsaveenvironment() {
	avm_push_envvalue(totalActuals);
	avm_pushenvvalue(pc+1);
	avm_push_envvalue(top+totalActuals+2);
	avm_push_envvalue(topsp);
}

case userfunc_m: {
	pc=func->data.funcVal;
	assert(pc<AVM_ENDING_PC);
	assert(code[pc].opcode==funcenter_v);
	break;
}

//funcenter
totalActuals=0;
userfunc* funcInfo=avm_getfuncinfo(pc);
topsp=top;
top=top-functionInfo->localSize;


//funcexit
void execute_funcexit(instruction* unused) {
	unsigned oldTop=top;
	top=avm_get_envvalue(topsp+AVM_SAVEDTOP_OFFSET);
	pc=avm_get_envvalue(topsp+AVM_SAVEDPC_OFFSET);
	topsp=avm_get_envvalue(topsp+AVM_SAVEDTOPSP_OFFSET);

	while(oldTop++<=top) //intentionally ignoring first
		avm_memcellclear(&stack[oldTop]);
}


//pusharg 
avm_assign(&stack[top], arg);
++totalActuals;
avm_dec_top();



}

*/