/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ID = 258,
     HBLOCK = 259,
     POUND = 260,
     STRING = 261,
     INCLUDE = 262,
     IMPORT = 263,
     INSERT = 264,
     CHARCONST = 265,
     NUM_INT = 266,
     NUM_FLOAT = 267,
     NUM_UNSIGNED = 268,
     NUM_LONG = 269,
     NUM_ULONG = 270,
     NUM_LONGLONG = 271,
     NUM_ULONGLONG = 272,
     NUM_BOOL = 273,
     TYPEDEF = 274,
     TYPE_INT = 275,
     TYPE_UNSIGNED = 276,
     TYPE_SHORT = 277,
     TYPE_LONG = 278,
     TYPE_FLOAT = 279,
     TYPE_DOUBLE = 280,
     TYPE_CHAR = 281,
     TYPE_WCHAR = 282,
     TYPE_VOID = 283,
     TYPE_SIGNED = 284,
     TYPE_BOOL = 285,
     TYPE_COMPLEX = 286,
     TYPE_TYPEDEF = 287,
     TYPE_RAW = 288,
     TYPE_NON_ISO_INT8 = 289,
     TYPE_NON_ISO_INT16 = 290,
     TYPE_NON_ISO_INT32 = 291,
     TYPE_NON_ISO_INT64 = 292,
     LPAREN = 293,
     RPAREN = 294,
     COMMA = 295,
     SEMI = 296,
     EXTERN = 297,
     INIT = 298,
     LBRACE = 299,
     RBRACE = 300,
     PERIOD = 301,
     CONST_QUAL = 302,
     VOLATILE = 303,
     REGISTER = 304,
     STRUCT = 305,
     UNION = 306,
     EQUAL = 307,
     SIZEOF = 308,
     MODULE = 309,
     LBRACKET = 310,
     RBRACKET = 311,
     ILLEGAL = 312,
     CONSTANT = 313,
     NAME = 314,
     RENAME = 315,
     NAMEWARN = 316,
     EXTEND = 317,
     PRAGMA = 318,
     FEATURE = 319,
     VARARGS = 320,
     ENUM = 321,
     CLASS = 322,
     TYPENAME = 323,
     PRIVATE = 324,
     PUBLIC = 325,
     PROTECTED = 326,
     COLON = 327,
     STATIC = 328,
     VIRTUAL = 329,
     FRIEND = 330,
     THROW = 331,
     CATCH = 332,
     EXPLICIT = 333,
     USING = 334,
     NAMESPACE = 335,
     NATIVE = 336,
     INLINE = 337,
     TYPEMAP = 338,
     EXCEPT = 339,
     ECHO = 340,
     APPLY = 341,
     CLEAR = 342,
     SWIGTEMPLATE = 343,
     FRAGMENT = 344,
     WARN = 345,
     LESSTHAN = 346,
     GREATERTHAN = 347,
     DELETE_KW = 348,
     LESSTHANOREQUALTO = 349,
     GREATERTHANOREQUALTO = 350,
     EQUALTO = 351,
     NOTEQUALTO = 352,
     QUESTIONMARK = 353,
     TYPES = 354,
     PARMS = 355,
     NONID = 356,
     DSTAR = 357,
     DCNOT = 358,
     TEMPLATE = 359,
     OPERATOR = 360,
     COPERATOR = 361,
     PARSETYPE = 362,
     PARSEPARM = 363,
     PARSEPARMS = 364,
     CAST = 365,
     LOR = 366,
     LAND = 367,
     OR = 368,
     XOR = 369,
     AND = 370,
     RSHIFT = 371,
     LSHIFT = 372,
     MINUS = 373,
     PLUS = 374,
     MODULO = 375,
     SLASH = 376,
     STAR = 377,
     LNOT = 378,
     NOT = 379,
     UMINUS = 380,
     DCOLON = 381
   };
#endif
/* Tokens.  */
#define ID 258
#define HBLOCK 259
#define POUND 260
#define STRING 261
#define INCLUDE 262
#define IMPORT 263
#define INSERT 264
#define CHARCONST 265
#define NUM_INT 266
#define NUM_FLOAT 267
#define NUM_UNSIGNED 268
#define NUM_LONG 269
#define NUM_ULONG 270
#define NUM_LONGLONG 271
#define NUM_ULONGLONG 272
#define NUM_BOOL 273
#define TYPEDEF 274
#define TYPE_INT 275
#define TYPE_UNSIGNED 276
#define TYPE_SHORT 277
#define TYPE_LONG 278
#define TYPE_FLOAT 279
#define TYPE_DOUBLE 280
#define TYPE_CHAR 281
#define TYPE_WCHAR 282
#define TYPE_VOID 283
#define TYPE_SIGNED 284
#define TYPE_BOOL 285
#define TYPE_COMPLEX 286
#define TYPE_TYPEDEF 287
#define TYPE_RAW 288
#define TYPE_NON_ISO_INT8 289
#define TYPE_NON_ISO_INT16 290
#define TYPE_NON_ISO_INT32 291
#define TYPE_NON_ISO_INT64 292
#define LPAREN 293
#define RPAREN 294
#define COMMA 295
#define SEMI 296
#define EXTERN 297
#define INIT 298
#define LBRACE 299
#define RBRACE 300
#define PERIOD 301
#define CONST_QUAL 302
#define VOLATILE 303
#define REGISTER 304
#define STRUCT 305
#define UNION 306
#define EQUAL 307
#define SIZEOF 308
#define MODULE 309
#define LBRACKET 310
#define RBRACKET 311
#define ILLEGAL 312
#define CONSTANT 313
#define NAME 314
#define RENAME 315
#define NAMEWARN 316
#define EXTEND 317
#define PRAGMA 318
#define FEATURE 319
#define VARARGS 320
#define ENUM 321
#define CLASS 322
#define TYPENAME 323
#define PRIVATE 324
#define PUBLIC 325
#define PROTECTED 326
#define COLON 327
#define STATIC 328
#define VIRTUAL 329
#define FRIEND 330
#define THROW 331
#define CATCH 332
#define EXPLICIT 333
#define USING 334
#define NAMESPACE 335
#define NATIVE 336
#define INLINE 337
#define TYPEMAP 338
#define EXCEPT 339
#define ECHO 340
#define APPLY 341
#define CLEAR 342
#define SWIGTEMPLATE 343
#define FRAGMENT 344
#define WARN 345
#define LESSTHAN 346
#define GREATERTHAN 347
#define DELETE_KW 348
#define LESSTHANOREQUALTO 349
#define GREATERTHANOREQUALTO 350
#define EQUALTO 351
#define NOTEQUALTO 352
#define QUESTIONMARK 353
#define TYPES 354
#define PARMS 355
#define NONID 356
#define DSTAR 357
#define DCNOT 358
#define TEMPLATE 359
#define OPERATOR 360
#define COPERATOR 361
#define PARSETYPE 362
#define PARSEPARM 363
#define PARSEPARMS 364
#define CAST 365
#define LOR 366
#define LAND 367
#define OR 368
#define XOR 369
#define AND 370
#define RSHIFT 371
#define LSHIFT 372
#define MINUS 373
#define PLUS 374
#define MODULO 375
#define SLASH 376
#define STAR 377
#define LNOT 378
#define NOT 379
#define UMINUS 380
#define DCOLON 381




/* Copy the first part of user declarations.  */
#line 16 "parser.y"


#define yylex yylex

char cvsroot_parser_y[] = "$Id: parser.y 12028 2010-05-14 18:46:20Z wsfulton $";

#include "swig.h"
#include "cparse.h"
#include "preprocessor.h"
#include <ctype.h>

/* We do this for portability */
#undef alloca
#define alloca malloc

/* -----------------------------------------------------------------------------
 *                               Externals
 * ----------------------------------------------------------------------------- */

int  yyparse();

/* NEW Variables */

static Node    *top = 0;      /* Top of the generated parse tree */
static int      unnamed = 0;  /* Unnamed datatype counter */
static Hash    *extendhash = 0;     /* Hash table of added methods */
static Hash    *classes = 0;        /* Hash table of classes */
static Symtab  *prev_symtab = 0;
static Node    *current_class = 0;
String  *ModuleName = 0;
static Node    *module_node = 0;
static String  *Classprefix = 0;  
static String  *Namespaceprefix = 0;
static int      inclass = 0;
static int      nested_template = 0; /* template class/function definition within a class */
static char    *last_cpptype = 0;
static int      inherit_list = 0;
static Parm    *template_parameters = 0;
static int      extendmode   = 0;
static int      compact_default_args = 0;
static int      template_reduce = 0;
static int      cparse_externc = 0;

static int      max_class_levels = 0;
static int      class_level = 0;
static Node   **class_decl = NULL;

/* -----------------------------------------------------------------------------
 *                            Assist Functions
 * ----------------------------------------------------------------------------- */


 
/* Called by the parser (yyparse) when an error is found.*/
static void yyerror (const char *e) {
  (void)e;
}

static Node *new_node(const_String_or_char_ptr tag) {
  Node *n = NewHash();
  set_nodeType(n,tag);
  Setfile(n,cparse_file);
  Setline(n,cparse_line);
  return n;
}

/* Copies a node.  Does not copy tree links or symbol table data (except for
   sym:name) */

static Node *copy_node(Node *n) {
  Node *nn;
  Iterator k;
  nn = NewHash();
  Setfile(nn,Getfile(n));
  Setline(nn,Getline(n));
  for (k = First(n); k.key; k = Next(k)) {
    String *ci;
    String *key = k.key;
    char *ckey = Char(key);
    if ((strcmp(ckey,"nextSibling") == 0) ||
	(strcmp(ckey,"previousSibling") == 0) ||
	(strcmp(ckey,"parentNode") == 0) ||
	(strcmp(ckey,"lastChild") == 0)) {
      continue;
    }
    if (Strncmp(key,"csym:",5) == 0) continue;
    /* We do copy sym:name.  For templates */
    if ((strcmp(ckey,"sym:name") == 0) || 
	(strcmp(ckey,"sym:weak") == 0) ||
	(strcmp(ckey,"sym:typename") == 0)) {
      String *ci = Copy(k.item);
      Setattr(nn,key, ci);
      Delete(ci);
      continue;
    }
    if (strcmp(ckey,"sym:symtab") == 0) {
      Setattr(nn,"sym:needs_symtab", "1");
    }
    /* We don't copy any other symbol table attributes */
    if (strncmp(ckey,"sym:",4) == 0) {
      continue;
    }
    /* If children.  We copy them recursively using this function */
    if (strcmp(ckey,"firstChild") == 0) {
      /* Copy children */
      Node *cn = k.item;
      while (cn) {
	Node *copy = copy_node(cn);
	appendChild(nn,copy);
	Delete(copy);
	cn = nextSibling(cn);
      }
      continue;
    }
    /* We don't copy the symbol table.  But we drop an attribute 
       requires_symtab so that functions know it needs to be built */

    if (strcmp(ckey,"symtab") == 0) {
      /* Node defined a symbol table. */
      Setattr(nn,"requires_symtab","1");
      continue;
    }
    /* Can't copy nodes */
    if (strcmp(ckey,"node") == 0) {
      continue;
    }
    if ((strcmp(ckey,"parms") == 0) || (strcmp(ckey,"pattern") == 0) || (strcmp(ckey,"throws") == 0)
	|| (strcmp(ckey,"kwargs") == 0)) {
      ParmList *pl = CopyParmList(k.item);
      Setattr(nn,key,pl);
      Delete(pl);
      continue;
    }
    /* Looks okay.  Just copy the data using Copy */
    ci = Copy(k.item);
    Setattr(nn, key, ci);
    Delete(ci);
  }
  return nn;
}

/* -----------------------------------------------------------------------------
 *                              Variables
 * ----------------------------------------------------------------------------- */

static char  *typemap_lang = 0;    /* Current language setting */

static int cplus_mode  = 0;
static String  *class_rename = 0;

/* C++ modes */

#define  CPLUS_PUBLIC    1
#define  CPLUS_PRIVATE   2
#define  CPLUS_PROTECTED 3

/* include types */
static int   import_mode = 0;

void SWIG_typemap_lang(const char *tm_lang) {
  typemap_lang = Swig_copy_string(tm_lang);
}

void SWIG_cparse_set_compact_default_args(int defargs) {
  compact_default_args = defargs;
}

int SWIG_cparse_template_reduce(int treduce) {
  template_reduce = treduce;
  return treduce;  
}

/* -----------------------------------------------------------------------------
 *                           Assist functions
 * ----------------------------------------------------------------------------- */

static int promote_type(int t) {
  if (t <= T_UCHAR || t == T_CHAR) return T_INT;
  return t;
}

/* Perform type-promotion for binary operators */
static int promote(int t1, int t2) {
  t1 = promote_type(t1);
  t2 = promote_type(t2);
  return t1 > t2 ? t1 : t2;
}

static String *yyrename = 0;

/* Forward renaming operator */

static String *resolve_node_scope(String *cname);


Hash *Swig_cparse_features(void) {
  static Hash   *features_hash = 0;
  if (!features_hash) features_hash = NewHash();
  return features_hash;
}

static String *feature_identifier_fix(String *s) {
  String *tp = SwigType_istemplate_templateprefix(s);
  if (tp) {
    String *ts, *ta, *tq;
    ts = SwigType_templatesuffix(s);
    ta = SwigType_templateargs(s);
    tq = Swig_symbol_type_qualify(ta,0);
    Append(tp,tq);
    Append(tp,ts);
    Delete(ts);
    Delete(ta);
    Delete(tq);
    return tp;
  } else {
    return NewString(s);
  }
}

/* Generate the symbol table name for an object */
/* This is a bit of a mess. Need to clean up */
static String *add_oldname = 0;



static String *make_name(Node *n, String *name,SwigType *decl) {
  int destructor = name && (*(Char(name)) == '~');

  if (yyrename) {
    String *s = NewString(yyrename);
    Delete(yyrename);
    yyrename = 0;
    if (destructor  && (*(Char(s)) != '~')) {
      Insert(s,0,"~");
    }
    return s;
  }

  if (!name) return 0;
  return Swig_name_make(n,Namespaceprefix,name,decl,add_oldname);
}

/* Generate an unnamed identifier */
static String *make_unnamed() {
  unnamed++;
  return NewStringf("$unnamed%d$",unnamed);
}

/* Return if the node is a friend declaration */
static int is_friend(Node *n) {
  return Cmp(Getattr(n,"storage"),"friend") == 0;
}

static int is_operator(String *name) {
  return Strncmp(name,"operator ", 9) == 0;
}


/* Add declaration list to symbol table */
static int  add_only_one = 0;

static void add_symbols(Node *n) {
  String *decl;
  String *wrn = 0;

  if (nested_template) {
    if (!(n && Equal(nodeType(n), "template"))) {
      return;
    }
    /* continue if template function, but not template class, declared within a class */
  }

  if (inclass && n) {
    cparse_normalize_void(n);
  }
  while (n) {
    String *symname = 0;
    /* for friends, we need to pop the scope once */
    String *old_prefix = 0;
    Symtab *old_scope = 0;
    int isfriend = inclass && is_friend(n);
    int iscdecl = Cmp(nodeType(n),"cdecl") == 0;
    int only_csymbol = 0;
    if (extendmode) {
      Setattr(n,"isextension","1");
    }
    
    if (inclass) {
      String *name = Getattr(n, "name");
      if (isfriend) {
	/* for friends, we need to add the scopename if needed */
	String *prefix = name ? Swig_scopename_prefix(name) : 0;
	old_prefix = Namespaceprefix;
	old_scope = Swig_symbol_popscope();
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	if (!prefix) {
	  if (name && !is_operator(name) && Namespaceprefix) {
	    String *nname = NewStringf("%s::%s", Namespaceprefix, name);
	    Setattr(n,"name",nname);
	    Delete(nname);
	  }
	} else {
	  Symtab *st = Swig_symbol_getscope(prefix);
	  String *ns = st ? Getattr(st,"name") : prefix;
	  String *base  = Swig_scopename_last(name);
	  String *nname = NewStringf("%s::%s", ns, base);
	  Setattr(n,"name",nname);
	  Delete(nname);
	  Delete(base);
	  Delete(prefix);
	}
	Namespaceprefix = 0;
      } else {
	/* for member functions, we need to remove the redundant
	   class scope if provided, as in
	   
	   struct Foo {
	   int Foo::method(int a);
	   };
	   
	*/
	String *prefix = name ? Swig_scopename_prefix(name) : 0;
	if (prefix) {
	  if (Classprefix && (Equal(prefix,Classprefix))) {
	    String *base = Swig_scopename_last(name);
	    Setattr(n,"name",base);
	    Delete(base);
	  }
	  Delete(prefix);
	}

        /*
	if (!Getattr(n,"parentNode") && class_level) set_parentNode(n,class_decl[class_level - 1]);
        */
	Setattr(n,"ismember","1");
      }
    }
    if (!isfriend && inclass) {
      if ((cplus_mode != CPLUS_PUBLIC)) {
	only_csymbol = 1;
	if (cplus_mode == CPLUS_PROTECTED) {
	  Setattr(n,"access", "protected");
	  only_csymbol = !Swig_need_protected(n);
	} else {
	  Setattr(n,"access", "private");
	  /* private are needed only when they are pure virtuals - why? */
	  if ((Cmp(Getattr(n,"storage"),"virtual") == 0) && (Cmp(Getattr(n,"value"),"0") == 0)) {
	    only_csymbol = 0;
	  }
	}
      } else {
	  Setattr(n,"access", "public");
      }
    }
    if (Getattr(n,"sym:name")) {
      n = nextSibling(n);
      continue;
    }
    decl = Getattr(n,"decl");
    if (!SwigType_isfunction(decl)) {
      String *name = Getattr(n,"name");
      String *makename = Getattr(n,"parser:makename");
      if (iscdecl) {	
	String *storage = Getattr(n, "storage");
	if (Cmp(storage,"typedef") == 0) {
	  Setattr(n,"kind","typedef");
	} else {
	  SwigType *type = Getattr(n,"type");
	  String *value = Getattr(n,"value");
	  Setattr(n,"kind","variable");
	  if (value && Len(value)) {
	    Setattr(n,"hasvalue","1");
	  }
	  if (type) {
	    SwigType *ty;
	    SwigType *tmp = 0;
	    if (decl) {
	      ty = tmp = Copy(type);
	      SwigType_push(ty,decl);
	    } else {
	      ty = type;
	    }
	    if (!SwigType_ismutable(ty)) {
	      SetFlag(n,"hasconsttype");
	      SetFlag(n,"feature:immutable");
	    }
	    if (tmp) Delete(tmp);
	  }
	  if (!type) {
	    Printf(stderr,"notype name %s\n", name);
	  }
	}
      }
      Swig_features_get(Swig_cparse_features(), Namespaceprefix, name, 0, n);
      if (makename) {
	symname = make_name(n, makename,0);
        Delattr(n,"parser:makename"); /* temporary information, don't leave it hanging around */
      } else {
        makename = name;
	symname = make_name(n, makename,0);
      }
      
      if (!symname) {
	symname = Copy(Getattr(n,"unnamed"));
      }
      if (symname) {
	wrn = Swig_name_warning(n, Namespaceprefix, symname,0);
      }
    } else {
      String *name = Getattr(n,"name");
      SwigType *fdecl = Copy(decl);
      SwigType *fun = SwigType_pop_function(fdecl);
      if (iscdecl) {	
	Setattr(n,"kind","function");
      }
      
      Swig_features_get(Swig_cparse_features(),Namespaceprefix,name,fun,n);

      symname = make_name(n, name,fun);
      wrn = Swig_name_warning(n, Namespaceprefix,symname,fun);
      
      Delete(fdecl);
      Delete(fun);
      
    }
    if (!symname) {
      n = nextSibling(n);
      continue;
    }
    if (only_csymbol || GetFlag(n,"feature:ignore")) {
      /* Only add to C symbol table and continue */
      Swig_symbol_add(0, n);
    } else if (strncmp(Char(symname),"$ignore",7) == 0) {
      char *c = Char(symname)+7;
      SetFlag(n,"feature:ignore");
      if (strlen(c)) {
	SWIG_WARN_NODE_BEGIN(n);
	Swig_warning(0,Getfile(n), Getline(n), "%s\n",c+1);
	SWIG_WARN_NODE_END(n);
      }
      Swig_symbol_add(0, n);
    } else {
      Node *c;
      if ((wrn) && (Len(wrn))) {
	String *metaname = symname;
	if (!Getmeta(metaname,"already_warned")) {
	  SWIG_WARN_NODE_BEGIN(n);
	  Swig_warning(0,Getfile(n),Getline(n), "%s\n", wrn);
	  SWIG_WARN_NODE_END(n);
	  Setmeta(metaname,"already_warned","1");
	}
      }
      c = Swig_symbol_add(symname,n);

      if (c != n) {
        /* symbol conflict attempting to add in the new symbol */
        if (Getattr(n,"sym:weak")) {
          Setattr(n,"sym:name",symname);
        } else {
          String *e = NewStringEmpty();
          String *en = NewStringEmpty();
          String *ec = NewStringEmpty();
          int redefined = Swig_need_redefined_warn(n,c,inclass);
          if (redefined) {
            Printf(en,"Identifier '%s' redefined (ignored)",symname);
            Printf(ec,"previous definition of '%s'",symname);
          } else {
            Printf(en,"Redundant redeclaration of '%s'",symname);
            Printf(ec,"previous declaration of '%s'",symname);
          }
          if (Cmp(symname,Getattr(n,"name"))) {
            Printf(en," (Renamed from '%s')", SwigType_namestr(Getattr(n,"name")));
          }
          Printf(en,",");
          if (Cmp(symname,Getattr(c,"name"))) {
            Printf(ec," (Renamed from '%s')", SwigType_namestr(Getattr(c,"name")));
          }
          Printf(ec,".");
	  SWIG_WARN_NODE_BEGIN(n);
          if (redefined) {
            Swig_warning(WARN_PARSE_REDEFINED,Getfile(n),Getline(n),"%s\n",en);
            Swig_warning(WARN_PARSE_REDEFINED,Getfile(c),Getline(c),"%s\n",ec);
          } else if (!is_friend(n) && !is_friend(c)) {
            Swig_warning(WARN_PARSE_REDUNDANT,Getfile(n),Getline(n),"%s\n",en);
            Swig_warning(WARN_PARSE_REDUNDANT,Getfile(c),Getline(c),"%s\n",ec);
          }
	  SWIG_WARN_NODE_END(n);
          Printf(e,"%s:%d:%s\n%s:%d:%s\n",Getfile(n),Getline(n),en,
                 Getfile(c),Getline(c),ec);
          Setattr(n,"error",e);
	  Delete(e);
          Delete(en);
          Delete(ec);
        }
      }
    }
    /* restore the class scope if needed */
    if (isfriend) {
      Swig_symbol_setscope(old_scope);
      if (old_prefix) {
	Delete(Namespaceprefix);
	Namespaceprefix = old_prefix;
      }
    }
    Delete(symname);

    if (add_only_one) return;
    n = nextSibling(n);
  }
}


/* add symbols a parse tree node copy */

static void add_symbols_copy(Node *n) {
  String *name;
  int    emode = 0;
  while (n) {
    char *cnodeType = Char(nodeType(n));

    if (strcmp(cnodeType,"access") == 0) {
      String *kind = Getattr(n,"kind");
      if (Strcmp(kind,"public") == 0) {
	cplus_mode = CPLUS_PUBLIC;
      } else if (Strcmp(kind,"private") == 0) {
	cplus_mode = CPLUS_PRIVATE;
      } else if (Strcmp(kind,"protected") == 0) {
	cplus_mode = CPLUS_PROTECTED;
      }
      n = nextSibling(n);
      continue;
    }

    add_oldname = Getattr(n,"sym:name");
    if ((add_oldname) || (Getattr(n,"sym:needs_symtab"))) {
      int old_inclass = -1;
      Node *old_current_class = 0;
      if (add_oldname) {
	DohIncref(add_oldname);
	/*  Disable this, it prevents %rename to work with templates */
	/* If already renamed, we used that name  */
	/*
	if (Strcmp(add_oldname, Getattr(n,"name")) != 0) {
	  Delete(yyrename);
	  yyrename = Copy(add_oldname);
	}
	*/
      }
      Delattr(n,"sym:needs_symtab");
      Delattr(n,"sym:name");

      add_only_one = 1;
      add_symbols(n);

      if (Getattr(n,"partialargs")) {
	Swig_symbol_cadd(Getattr(n,"partialargs"),n);
      }
      add_only_one = 0;
      name = Getattr(n,"name");
      if (Getattr(n,"requires_symtab")) {
	Swig_symbol_newscope();
	Swig_symbol_setscopename(name);
	Delete(Namespaceprefix);
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
      }
      if (strcmp(cnodeType,"class") == 0) {
	old_inclass = inclass;
	inclass = 1;
	old_current_class = current_class;
	current_class = n;
	if (Strcmp(Getattr(n,"kind"),"class") == 0) {
	  cplus_mode = CPLUS_PRIVATE;
	} else {
	  cplus_mode = CPLUS_PUBLIC;
	}
      }
      if (strcmp(cnodeType,"extend") == 0) {
	emode = cplus_mode;
	cplus_mode = CPLUS_PUBLIC;
      }
      add_symbols_copy(firstChild(n));
      if (strcmp(cnodeType,"extend") == 0) {
	cplus_mode = emode;
      }
      if (Getattr(n,"requires_symtab")) {
	Setattr(n,"symtab", Swig_symbol_popscope());
	Delattr(n,"requires_symtab");
	Delete(Namespaceprefix);
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
      }
      if (add_oldname) {
	Delete(add_oldname);
	add_oldname = 0;
      }
      if (strcmp(cnodeType,"class") == 0) {
	inclass = old_inclass;
	current_class = old_current_class;
      }
    } else {
      if (strcmp(cnodeType,"extend") == 0) {
	emode = cplus_mode;
	cplus_mode = CPLUS_PUBLIC;
      }
      add_symbols_copy(firstChild(n));
      if (strcmp(cnodeType,"extend") == 0) {
	cplus_mode = emode;
      }
    }
    n = nextSibling(n);
  }
}

/* Extension merge.  This function is used to handle the %extend directive
   when it appears before a class definition.   To handle this, the %extend
   actually needs to take precedence.  Therefore, we will selectively nuke symbols
   from the current symbol table, replacing them with the added methods */

static void merge_extensions(Node *cls, Node *am) {
  Node *n;
  Node *csym;

  n = firstChild(am);
  while (n) {
    String *symname;
    if (Strcmp(nodeType(n),"constructor") == 0) {
      symname = Getattr(n,"sym:name");
      if (symname) {
	if (Strcmp(symname,Getattr(n,"name")) == 0) {
	  /* If the name and the sym:name of a constructor are the same,
             then it hasn't been renamed.  However---the name of the class
             itself might have been renamed so we need to do a consistency
             check here */
	  if (Getattr(cls,"sym:name")) {
	    Setattr(n,"sym:name", Getattr(cls,"sym:name"));
	  }
	}
      } 
    }

    symname = Getattr(n,"sym:name");
    DohIncref(symname);
    if ((symname) && (!Getattr(n,"error"))) {
      /* Remove node from its symbol table */
      Swig_symbol_remove(n);
      csym = Swig_symbol_add(symname,n);
      if (csym != n) {
	/* Conflict with previous definition.  Nuke previous definition */
	String *e = NewStringEmpty();
	String *en = NewStringEmpty();
	String *ec = NewStringEmpty();
	Printf(ec,"Identifier '%s' redefined by %%extend (ignored),",symname);
	Printf(en,"%%extend definition of '%s'.",symname);
	SWIG_WARN_NODE_BEGIN(n);
	Swig_warning(WARN_PARSE_REDEFINED,Getfile(csym),Getline(csym),"%s\n",ec);
	Swig_warning(WARN_PARSE_REDEFINED,Getfile(n),Getline(n),"%s\n",en);
	SWIG_WARN_NODE_END(n);
	Printf(e,"%s:%d:%s\n%s:%d:%s\n",Getfile(csym),Getline(csym),ec, 
	       Getfile(n),Getline(n),en);
	Setattr(csym,"error",e);
	Delete(e);
	Delete(en);
	Delete(ec);
	Swig_symbol_remove(csym);              /* Remove class definition */
	Swig_symbol_add(symname,n);            /* Insert extend definition */
      }
    }
    n = nextSibling(n);
  }
}

static void append_previous_extension(Node *cls, Node *am) {
  Node *n, *ne;
  Node *pe = 0;
  Node *ae = 0;

  if (!am) return;
  
  n = firstChild(am);
  while (n) {
    ne = nextSibling(n);
    set_nextSibling(n,0);
    /* typemaps and fragments need to be prepended */
    if (((Cmp(nodeType(n),"typemap") == 0) || (Cmp(nodeType(n),"fragment") == 0)))  {
      if (!pe) pe = new_node("extend");
      appendChild(pe, n);
    } else {
      if (!ae) ae = new_node("extend");
      appendChild(ae, n);
    }    
    n = ne;
  }
  if (pe) prependChild(cls,pe);
  if (ae) appendChild(cls,ae);
}
 

/* Check for unused %extend.  Special case, don't report unused
   extensions for templates */
 
static void check_extensions() {
  Iterator ki;

  if (!extendhash) return;
  for (ki = First(extendhash); ki.key; ki = Next(ki)) {
    if (!Strchr(ki.key,'<')) {
      SWIG_WARN_NODE_BEGIN(ki.item);
      Swig_warning(WARN_PARSE_EXTEND_UNDEF,Getfile(ki.item), Getline(ki.item), "%%extend defined for an undeclared class %s.\n", ki.key);
      SWIG_WARN_NODE_END(ki.item);
    }
  }
}

/* Check a set of declarations to see if any are pure-abstract */

static List *pure_abstract(Node *n) {
  List *abs = 0;
  while (n) {
    if (Cmp(nodeType(n),"cdecl") == 0) {
      String *decl = Getattr(n,"decl");
      if (SwigType_isfunction(decl)) {
	String *init = Getattr(n,"value");
	if (Cmp(init,"0") == 0) {
	  if (!abs) {
	    abs = NewList();
	  }
	  Append(abs,n);
	  Setattr(n,"abstract","1");
	}
      }
    } else if (Cmp(nodeType(n),"destructor") == 0) {
      if (Cmp(Getattr(n,"value"),"0") == 0) {
	if (!abs) {
	  abs = NewList();
	}
	Append(abs,n);
	Setattr(n,"abstract","1");
      }
    }
    n = nextSibling(n);
  }
  return abs;
}

/* Make a classname */

static String *make_class_name(String *name) {
  String *nname = 0;
  String *prefix;
  if (Namespaceprefix) {
    nname= NewStringf("%s::%s", Namespaceprefix, name);
  } else {
    nname = NewString(name);
  }
  prefix = SwigType_istemplate_templateprefix(nname);
  if (prefix) {
    String *args, *qargs;
    args   = SwigType_templateargs(nname);
    qargs  = Swig_symbol_type_qualify(args,0);
    Append(prefix,qargs);
    Delete(nname);
    Delete(args);
    Delete(qargs);
    nname = prefix;
  }
  return nname;
}

static List *make_inherit_list(String *clsname, List *names) {
  int i, ilen;
  String *derived;
  List *bases = NewList();

  if (Namespaceprefix) derived = NewStringf("%s::%s", Namespaceprefix,clsname);
  else derived = NewString(clsname);

  ilen = Len(names);
  for (i = 0; i < ilen; i++) {
    Node *s;
    String *base;
    String *n = Getitem(names,i);
    /* Try to figure out where this symbol is */
    s = Swig_symbol_clookup(n,0);
    if (s) {
      while (s && (Strcmp(nodeType(s),"class") != 0)) {
	/* Not a class.  Could be a typedef though. */
	String *storage = Getattr(s,"storage");
	if (storage && (Strcmp(storage,"typedef") == 0)) {
	  String *nn = Getattr(s,"type");
	  s = Swig_symbol_clookup(nn,Getattr(s,"sym:symtab"));
	} else {
	  break;
	}
      }
      if (s && ((Strcmp(nodeType(s),"class") == 0) || (Strcmp(nodeType(s),"template") == 0))) {
	String *q = Swig_symbol_qualified(s);
	Append(bases,s);
	if (q) {
	  base = NewStringf("%s::%s", q, Getattr(s,"name"));
	  Delete(q);
	} else {
	  base = NewString(Getattr(s,"name"));
	}
      } else {
	base = NewString(n);
      }
    } else {
      base = NewString(n);
    }
    if (base) {
      Swig_name_inherit(base,derived);
      Delete(base);
    }
  }
  return bases;
}

/* If the class name is qualified.  We need to create or lookup namespace entries */

static Symtab *set_scope_to_global() {
  Symtab *symtab = Swig_symbol_global_scope();
  Swig_symbol_setscope(symtab);
  return symtab;
}
 
/* Remove the block braces, { and }, if the 'noblock' attribute is set.
 * Node *kw can be either a Hash or Parmlist. */
static String *remove_block(Node *kw, const String *inputcode) {
  String *modified_code = 0;
  while (kw) {
   String *name = Getattr(kw,"name");
   if (name && (Cmp(name,"noblock") == 0)) {
     char *cstr = Char(inputcode);
     size_t len = Len(inputcode);
     if (len && cstr[0] == '{') {
       --len; ++cstr; 
       if (len && cstr[len - 1] == '}') { --len; }
       /* we now remove the extra spaces */
       while (len && isspace((int)cstr[0])) { --len; ++cstr; }
       while (len && isspace((int)cstr[len - 1])) { --len; }
       modified_code = NewStringWithSize(cstr, len);
       break;
     }
   }
   kw = nextSibling(kw);
  }
  return modified_code;
}


static Node *nscope = 0;
static Node *nscope_inner = 0;

/* Remove the scope prefix from cname and return the base name without the prefix.
 * The scopes specified in the prefix are found, or created in the current namespace.
 * So ultimately the scope is changed to that required for the base name.
 * For example AA::BB::CC as input returns CC and creates the namespace AA then inner 
 * namespace BB in the current scope. If no scope separator (::) in the input, then nothing happens! */
static String *resolve_node_scope(String *cname) {
  Symtab *gscope = 0;
  nscope = 0;
  nscope_inner = 0;  
  if (Swig_scopename_check(cname)) {
    Node   *ns;
    String *prefix = Swig_scopename_prefix(cname);
    String *base = Swig_scopename_last(cname);
    if (prefix && (Strncmp(prefix,"::",2) == 0)) {
      /* Use the global scope */
      String *nprefix = NewString(Char(prefix)+2);
      Delete(prefix);
      prefix= nprefix;
      gscope = set_scope_to_global();
    }    
    if (!prefix || (Len(prefix) == 0)) {
      /* Use the global scope, but we need to add a 'global' namespace.  */
      if (!gscope) gscope = set_scope_to_global();
      /* note that this namespace is not the "unnamed" one,
	 and we don't use Setattr(nscope,"name", ""),
	 because the unnamed namespace is private */
      nscope = new_node("namespace");
      Setattr(nscope,"symtab", gscope);;
      nscope_inner = nscope;
      return base;
    }
    /* Try to locate the scope */
    ns = Swig_symbol_clookup(prefix,0);
    if (!ns) {
      Swig_error(cparse_file,cparse_line,"Undefined scope '%s'\n", prefix);
    } else {
      Symtab *nstab = Getattr(ns,"symtab");
      if (!nstab) {
	Swig_error(cparse_file,cparse_line,
		   "'%s' is not defined as a valid scope.\n", prefix);
	ns = 0;
      } else {
	/* Check if the node scope is the current scope */
	String *tname = Swig_symbol_qualifiedscopename(0);
	String *nname = Swig_symbol_qualifiedscopename(nstab);
	if (tname && (Strcmp(tname,nname) == 0)) {
	  ns = 0;
	  cname = base;
	}
	Delete(tname);
	Delete(nname);
      }
      if (ns) {
	/* we will try to create a new node using the namespaces we
	   can find in the scope name */
	List *scopes;
	String *sname;
	Iterator si;
	String *name = NewString(prefix);
	scopes = NewList();
	while (name) {
	  String *base = Swig_scopename_last(name);
	  String *tprefix = Swig_scopename_prefix(name);
	  Insert(scopes,0,base);
	  Delete(base);
	  Delete(name);
	  name = tprefix;
	}
	for (si = First(scopes); si.item; si = Next(si)) {
	  Node *ns1,*ns2;
	  sname = si.item;
	  ns1 = Swig_symbol_clookup(sname,0);
	  assert(ns1);
	  if (Strcmp(nodeType(ns1),"namespace") == 0) {
	    if (Getattr(ns1,"alias")) {
	      ns1 = Getattr(ns1,"namespace");
	    }
	  } else {
	    /* now this last part is a class */
	    si = Next(si);
	    ns1 = Swig_symbol_clookup(sname,0);
	    /*  or a nested class tree, which is unrolled here */
	    for (; si.item; si = Next(si)) {
	      if (si.item) {
		Printf(sname,"::%s",si.item);
	      }
	    }
	    /* we get the 'inner' class */
	    nscope_inner = Swig_symbol_clookup(sname,0);
	    /* set the scope to the inner class */
	    Swig_symbol_setscope(Getattr(nscope_inner,"symtab"));
	    /* save the last namespace prefix */
	    Delete(Namespaceprefix);
	    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	    /* and return the node name, including the inner class prefix */
	    break;
	  }
	  /* here we just populate the namespace tree as usual */
	  ns2 = new_node("namespace");
	  Setattr(ns2,"name",sname);
	  Setattr(ns2,"symtab", Getattr(ns1,"symtab"));
	  add_symbols(ns2);
	  Swig_symbol_setscope(Getattr(ns1,"symtab"));
	  Delete(Namespaceprefix);
	  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	  if (nscope_inner) {
	    if (Getattr(nscope_inner,"symtab") != Getattr(ns2,"symtab")) {
	      appendChild(nscope_inner,ns2);
	      Delete(ns2);
	    }
	  }
	  nscope_inner = ns2;
	  if (!nscope) nscope = ns2;
	}
	cname = base;
	Delete(scopes);
      }
    }
    Delete(prefix);
  }
  return cname;
}
 


/* Structures for handling code fragments built for nested classes */

typedef struct Nested {
  String   *code;        /* Associated code fragment */
  int      line;         /* line number where it starts */
  const char *name;      /* Name associated with this nested class */
  const char *kind;      /* Kind of class */
  int      unnamed;      /* unnamed class */
  SwigType *type;        /* Datatype associated with the name */
  struct Nested   *next; /* Next code fragment in list */
} Nested;

/* Some internal variables for saving nested class information */

static Nested      *nested_list = 0;

/* Add a function to the nested list */

static void add_nested(Nested *n) {
  if (!nested_list) {
    nested_list = n;
  } else {
    Nested *n1 = nested_list;
    while (n1->next)
      n1 = n1->next;
    n1->next = n;
  }
}

/* -----------------------------------------------------------------------------
 * nested_new_struct()
 *
 * Nested struct handling for C code only creates a global struct from the nested struct.
 *
 * Nested structure. This is a sick "hack". If we encounter
 * a nested structure, we're going to grab the text of its definition and
 * feed it back into the scanner.  In the meantime, we need to grab
 * variable declaration information and generate the associated wrapper
 * code later.  Yikes!
 *
 * This really only works in a limited sense.   Since we use the
 * code attached to the nested class to generate both C code
 * it can't have any SWIG directives in it.  It also needs to be parsable
 * by SWIG or this whole thing is going to puke.
 * ----------------------------------------------------------------------------- */

static void nested_new_struct(const char *kind, String *struct_code, Node *cpp_opt_declarators) {
  String *name;
  String *decl;

  /* Create a new global struct declaration which is just a copy of the nested struct */
  Nested *nested = (Nested *) malloc(sizeof(Nested));
  Nested *n = nested;

  name = Getattr(cpp_opt_declarators, "name");
  decl = Getattr(cpp_opt_declarators, "decl");

  n->code = NewStringEmpty();
  Printv(n->code, "typedef ", kind, " ", struct_code, " $classname_", name, ";\n", NIL);
  n->name = Swig_copy_string(Char(name));
  n->line = cparse_start_line;
  n->type = NewStringEmpty();
  n->kind = kind;
  n->unnamed = 0;
  SwigType_push(n->type, decl);
  n->next = 0;

  /* Repeat for any multiple instances of the nested struct */
  {
    Node *p = cpp_opt_declarators;
    p = nextSibling(p);
    while (p) {
      Nested *nn = (Nested *) malloc(sizeof(Nested));

      name = Getattr(p, "name");
      decl = Getattr(p, "decl");

      nn->code = NewStringEmpty();
      Printv(nn->code, "typedef ", kind, " ", struct_code, " $classname_", name, ";\n", NIL);
      nn->name = Swig_copy_string(Char(name));
      nn->line = cparse_start_line;
      nn->type = NewStringEmpty();
      nn->kind = kind;
      nn->unnamed = 0;
      SwigType_push(nn->type, decl);
      nn->next = 0;
      n->next = nn;
      n = nn;
      p = nextSibling(p);
    }
  }

  add_nested(nested);
}

/* -----------------------------------------------------------------------------
 * nested_forward_declaration()
 * 
 * Nested struct handling for C++ code only.
 *
 * Treat the nested class/struct/union as a forward declaration until a proper 
 * nested class solution is implemented.
 * ----------------------------------------------------------------------------- */

static Node *nested_forward_declaration(const char *storage, const char *kind, String *sname, const char *name, Node *cpp_opt_declarators) {
  Node *nn = 0;
  int warned = 0;

  if (sname) {
    /* Add forward declaration of the nested type */
    Node *n = new_node("classforward");
    Setfile(n, cparse_file);
    Setline(n, cparse_line);
    Setattr(n, "kind", kind);
    Setattr(n, "name", sname);
    Setattr(n, "storage", storage);
    Setattr(n, "sym:weak", "1");
    add_symbols(n);
    nn = n;
  }

  /* Add any variable instances. Also add in any further typedefs of the nested type.
     Note that anonymous typedefs (eg typedef struct {...} a, b;) are treated as class forward declarations */
  if (cpp_opt_declarators) {
    int storage_typedef = (storage && (strcmp(storage, "typedef") == 0));
    int variable_of_anonymous_type = !sname && !storage_typedef;
    if (!variable_of_anonymous_type) {
      int anonymous_typedef = !sname && (storage && (strcmp(storage, "typedef") == 0));
      Node *n = cpp_opt_declarators;
      SwigType *type = NewString(name);
      while (n) {
	Setattr(n, "type", type);
	Setattr(n, "storage", storage);
	if (anonymous_typedef) {
	  Setattr(n, "nodeType", "classforward");
	  Setattr(n, "sym:weak", "1");
	}
	n = nextSibling(n);
      }
      Delete(type);
      add_symbols(cpp_opt_declarators);

      if (nn) {
	set_nextSibling(nn, cpp_opt_declarators);
      } else {
	nn = cpp_opt_declarators;
      }
    }
  }

  if (nn && Equal(nodeType(nn), "classforward")) {
    Node *n = nn;
    if (GetFlag(n, "feature:nestedworkaround")) {
      Swig_symbol_remove(n);
      nn = 0;
      warned = 1;
    } else {
      SWIG_WARN_NODE_BEGIN(n);
      Swig_warning(WARN_PARSE_NAMED_NESTED_CLASS, cparse_file, cparse_line,"Nested %s not currently supported (%s ignored)\n", kind, sname ? sname : name);
      SWIG_WARN_NODE_END(n);
      warned = 1;
    }
  }

  if (!warned)
    Swig_warning(WARN_PARSE_UNNAMED_NESTED_CLASS, cparse_file, cparse_line, "Nested %s not currently supported (ignored).\n", kind);

  return nn;
}

/* Strips C-style and C++-style comments from string in-place. */
static void strip_comments(char *string) {
  int state = 0; /* 
                  * 0 - not in comment
                  * 1 - in c-style comment
                  * 2 - in c++-style comment
                  * 3 - in string
                  * 4 - after reading / not in comments
                  * 5 - after reading * in c-style comments
                  * 6 - after reading \ in strings
                  */
  char * c = string;
  while (*c) {
    switch (state) {
    case 0:
      if (*c == '\"')
        state = 3;
      else if (*c == '/')
        state = 4;
      break;
    case 1:
      if (*c == '*')
        state = 5;
      *c = ' ';
      break;
    case 2:
      if (*c == '\n')
        state = 0;
      else
        *c = ' ';
      break;
    case 3:
      if (*c == '\"')
        state = 0;
      else if (*c == '\\')
        state = 6;
      break;
    case 4:
      if (*c == '/') {
        *(c-1) = ' ';
        *c = ' ';
        state = 2;
      } else if (*c == '*') {
        *(c-1) = ' ';
        *c = ' ';
        state = 1;
      } else
        state = 0;
      break;
    case 5:
      if (*c == '/')
        state = 0;
      else 
        state = 1;
      *c = ' ';
      break;
    case 6:
      state = 3;
      break;
    }
    ++c;
  }
}

/* Dump all of the nested class declarations to the inline processor
 * However.  We need to do a few name replacements and other munging
 * first.  This function must be called before closing a class! */

static Node *dump_nested(const char *parent) {
  Nested *n,*n1;
  Node *ret = 0;
  Node *last = 0;
  n = nested_list;
  if (!parent) {
    nested_list = 0;
    return 0;
  }
  while (n) {
    Node *retx;
    SwigType *nt;
    /* Token replace the name of the parent class */
    Replace(n->code, "$classname", parent, DOH_REPLACE_ANY);

    /* Fix up the name of the datatype (for building typedefs and other stuff) */
    Append(n->type,parent);
    Append(n->type,"_");
    Append(n->type,n->name);

    /* Add the appropriate declaration to the C++ processor */
    retx = new_node("cdecl");
    Setattr(retx,"name",n->name);
    nt = Copy(n->type);
    Setattr(retx,"type",nt);
    Delete(nt);
    Setattr(retx,"nested",parent);
    if (n->unnamed) {
      Setattr(retx,"unnamed","1");
    }
    
    add_symbols(retx);
    if (ret) {
      set_nextSibling(last, retx);
      Delete(retx);
    } else {
      ret = retx;
    }
    last = retx;

    /* Strip comments - further code may break in presence of comments. */
    strip_comments(Char(n->code));

    /* Make all SWIG created typedef structs/unions/classes unnamed else 
       redefinition errors occur - nasty hack alert.*/

    {
      const char* types_array[3] = {"struct", "union", "class"};
      int i;
      for (i=0; i<3; i++) {
	char* code_ptr = Char(n->code);
	while (code_ptr) {
	  /* Replace struct name (as in 'struct name {...}' ) with whitespace
	     name will be between struct and opening brace */
	
	  code_ptr = strstr(code_ptr, types_array[i]);
	  if (code_ptr) {
	    char *open_bracket_pos;
	    code_ptr += strlen(types_array[i]);
	    open_bracket_pos = strchr(code_ptr, '{');
	    if (open_bracket_pos) { 
	      /* Make sure we don't have something like struct A a; */
	      char* semi_colon_pos = strchr(code_ptr, ';');
	      if (!(semi_colon_pos && (semi_colon_pos < open_bracket_pos)))
		while (code_ptr < open_bracket_pos)
		  *code_ptr++ = ' ';
	    }
	  }
	}
      }
    }
    
    {
      /* Remove SWIG directive %constant which may be left in the SWIG created typedefs */
      char* code_ptr = Char(n->code);
      while (code_ptr) {
	code_ptr = strstr(code_ptr, "%constant");
	if (code_ptr) {
	  char* directive_end_pos = strchr(code_ptr, ';');
	  if (directive_end_pos) { 
            while (code_ptr <= directive_end_pos)
              *code_ptr++ = ' ';
	  }
	}
      }
    }
    {
      Node *newnode = new_node("insert");
      String *code = NewStringEmpty();
      Wrapper_pretty_print(n->code, code);
      Setattr(newnode,"code", code);
      Delete(code);
      set_nextSibling(last, newnode);
      Delete(newnode);      
      last = newnode;
    }
      
    /* Dump the code to the scanner */
    start_inline(Char(Getattr(last, "code")),n->line);

    n1 = n->next;
    Delete(n->code);
    free(n);
    n = n1;
  }
  nested_list = 0;
  return ret;
}

Node *Swig_cparse(File *f) {
  scanner_file(f);
  top = 0;
  yyparse();
  return top;
}

static void single_new_feature(const char *featurename, String *val, Hash *featureattribs, char *declaratorid, SwigType *type, ParmList *declaratorparms, String *qualifier) {
  String *fname;
  String *name;
  String *fixname;
  SwigType *t = Copy(type);

  /* Printf(stdout, "single_new_feature: [%s] [%s] [%s] [%s] [%s] [%s]\n", featurename, val, declaratorid, t, ParmList_str_defaultargs(declaratorparms), qualifier); */

  fname = NewStringf("feature:%s",featurename);
  if (declaratorid) {
    fixname = feature_identifier_fix(declaratorid);
  } else {
    fixname = NewStringEmpty();
  }
  if (Namespaceprefix) {
    name = NewStringf("%s::%s",Namespaceprefix, fixname);
  } else {
    name = fixname;
  }

  if (declaratorparms) Setmeta(val,"parms",declaratorparms);
  if (!Len(t)) t = 0;
  if (t) {
    if (qualifier) SwigType_push(t,qualifier);
    if (SwigType_isfunction(t)) {
      SwigType *decl = SwigType_pop_function(t);
      if (SwigType_ispointer(t)) {
	String *nname = NewStringf("*%s",name);
	Swig_feature_set(Swig_cparse_features(), nname, decl, fname, val, featureattribs);
	Delete(nname);
      } else {
	Swig_feature_set(Swig_cparse_features(), name, decl, fname, val, featureattribs);
      }
      Delete(decl);
    } else if (SwigType_ispointer(t)) {
      String *nname = NewStringf("*%s",name);
      Swig_feature_set(Swig_cparse_features(),nname,0,fname,val, featureattribs);
      Delete(nname);
    }
  } else {
    /* Global feature, that is, feature not associated with any particular symbol */
    Swig_feature_set(Swig_cparse_features(),name,0,fname,val, featureattribs);
  }
  Delete(fname);
  Delete(name);
}

/* Add a new feature to the Hash. Additional features are added if the feature has a parameter list (declaratorparms)
 * and one or more of the parameters have a default argument. An extra feature is added for each defaulted parameter,
 * simulating the equivalent overloaded method. */
static void new_feature(const char *featurename, String *val, Hash *featureattribs, char *declaratorid, SwigType *type, ParmList *declaratorparms, String *qualifier) {

  ParmList *declparms = declaratorparms;

  /* remove the { and } braces if the noblock attribute is set */
  String *newval = remove_block(featureattribs, val);
  val = newval ? newval : val;

  /* Add the feature */
  single_new_feature(featurename, val, featureattribs, declaratorid, type, declaratorparms, qualifier);

  /* Add extra features if there are default parameters in the parameter list */
  if (type) {
    while (declparms) {
      if (ParmList_has_defaultargs(declparms)) {

        /* Create a parameter list for the new feature by copying all
           but the last (defaulted) parameter */
        ParmList* newparms = CopyParmListMax(declparms, ParmList_len(declparms)-1);

        /* Create new declaration - with the last parameter removed */
        SwigType *newtype = Copy(type);
        Delete(SwigType_pop_function(newtype)); /* remove the old parameter list from newtype */
        SwigType_add_function(newtype,newparms);

        single_new_feature(featurename, Copy(val), featureattribs, declaratorid, newtype, newparms, qualifier);
        declparms = newparms;
      } else {
        declparms = 0;
      }
    }
  }
}

/* check if a function declaration is a plain C object */
static int is_cfunction(Node *n) {
  if (!cparse_cplusplus || cparse_externc) return 1;
  if (Cmp(Getattr(n,"storage"),"externc") == 0) {
    return 1;
  }
  return 0;
}

/* If the Node is a function with parameters, check to see if any of the parameters
 * have default arguments. If so create a new function for each defaulted argument. 
 * The additional functions form a linked list of nodes with the head being the original Node n. */
static void default_arguments(Node *n) {
  Node *function = n;

  if (function) {
    ParmList *varargs = Getattr(function,"feature:varargs");
    if (varargs) {
      /* Handles the %varargs directive by looking for "feature:varargs" and 
       * substituting ... with an alternative set of arguments.  */
      Parm     *p = Getattr(function,"parms");
      Parm     *pp = 0;
      while (p) {
	SwigType *t = Getattr(p,"type");
	if (Strcmp(t,"v(...)") == 0) {
	  if (pp) {
	    ParmList *cv = Copy(varargs);
	    set_nextSibling(pp,cv);
	    Delete(cv);
	  } else {
	    ParmList *cv =  Copy(varargs);
	    Setattr(function,"parms", cv);
	    Delete(cv);
	  }
	  break;
	}
	pp = p;
	p = nextSibling(p);
      }
    }

    /* Do not add in functions if kwargs is being used or if user wants old default argument wrapping
       (one wrapped method per function irrespective of number of default arguments) */
    if (compact_default_args 
	|| is_cfunction(function) 
	|| GetFlag(function,"feature:compactdefaultargs") 
	|| GetFlag(function,"feature:kwargs")) {
      ParmList *p = Getattr(function,"parms");
      if (p) 
        Setattr(p,"compactdefargs", "1"); /* mark parameters for special handling */
      function = 0; /* don't add in extra methods */
    }
  }

  while (function) {
    ParmList *parms = Getattr(function,"parms");
    if (ParmList_has_defaultargs(parms)) {

      /* Create a parameter list for the new function by copying all
         but the last (defaulted) parameter */
      ParmList* newparms = CopyParmListMax(parms,ParmList_len(parms)-1);

      /* Create new function and add to symbol table */
      {
	SwigType *ntype = Copy(nodeType(function));
	char *cntype = Char(ntype);
        Node *new_function = new_node(ntype);
        SwigType *decl = Copy(Getattr(function,"decl"));
        int constqualifier = SwigType_isconst(decl);
	String *ccode = Copy(Getattr(function,"code"));
	String *cstorage = Copy(Getattr(function,"storage"));
	String *cvalue = Copy(Getattr(function,"value"));
	SwigType *ctype = Copy(Getattr(function,"type"));
	String *cthrow = Copy(Getattr(function,"throw"));

        Delete(SwigType_pop_function(decl)); /* remove the old parameter list from decl */
        SwigType_add_function(decl,newparms);
        if (constqualifier)
          SwigType_add_qualifier(decl,"const");

        Setattr(new_function,"name", Getattr(function,"name"));
        Setattr(new_function,"code", ccode);
        Setattr(new_function,"decl", decl);
        Setattr(new_function,"parms", newparms);
        Setattr(new_function,"storage", cstorage);
        Setattr(new_function,"value", cvalue);
        Setattr(new_function,"type", ctype);
        Setattr(new_function,"throw", cthrow);

	Delete(ccode);
	Delete(cstorage);
	Delete(cvalue);
	Delete(ctype);
	Delete(cthrow);
	Delete(decl);

        {
          Node *throws = Getattr(function,"throws");
	  ParmList *pl = CopyParmList(throws);
          if (throws) Setattr(new_function,"throws",pl);
	  Delete(pl);
        }

        /* copy specific attributes for global (or in a namespace) template functions - these are not templated class methods */
        if (strcmp(cntype,"template") == 0) {
          Node *templatetype = Getattr(function,"templatetype");
          Node *symtypename = Getattr(function,"sym:typename");
          Parm *templateparms = Getattr(function,"templateparms");
          if (templatetype) {
	    Node *tmp = Copy(templatetype);
	    Setattr(new_function,"templatetype",tmp);
	    Delete(tmp);
	  }
          if (symtypename) {
	    Node *tmp = Copy(symtypename);
	    Setattr(new_function,"sym:typename",tmp);
	    Delete(tmp);
	  }
          if (templateparms) {
	    Parm *tmp = CopyParmList(templateparms);
	    Setattr(new_function,"templateparms",tmp);
	    Delete(tmp);
	  }
        } else if (strcmp(cntype,"constructor") == 0) {
          /* only copied for constructors as this is not a user defined feature - it is hard coded in the parser */
          if (GetFlag(function,"feature:new")) SetFlag(new_function,"feature:new");
        }

        add_symbols(new_function);
        /* mark added functions as ones with overloaded parameters and point to the parsed method */
        Setattr(new_function,"defaultargs", n);

        /* Point to the new function, extending the linked list */
        set_nextSibling(function, new_function);
	Delete(new_function);
        function = new_function;
	
	Delete(ntype);
      }
    } else {
      function = 0;
    }
  }
}

/* -----------------------------------------------------------------------------
 * tag_nodes()
 *
 * Used by the parser to mark subtypes with extra information.
 * ----------------------------------------------------------------------------- */

static void tag_nodes(Node *n, const_String_or_char_ptr attrname, DOH *value) {
  while (n) {
    Setattr(n, attrname, value);
    tag_nodes(firstChild(n), attrname, value);
    n = nextSibling(n);
  }
}



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 1592 "parser.y"
{
  char  *id;
  List  *bases;
  struct Define {
    String *val;
    String *rawval;
    int     type;
    String *qualifier;
    String *bitfield;
    Parm   *throws;
    String *throwf;
  } dtype;
  struct {
    char *type;
    String *filename;
    int   line;
  } loc;
  struct {
    char      *id;
    SwigType  *type;
    String    *defarg;
    ParmList  *parms;
    short      have_parms;
    ParmList  *throws;
    String    *throwf;
  } decl;
  Parm         *tparms;
  struct {
    String     *method;
    Hash       *kwargs;
  } tmap;
  struct {
    String     *type;
    String     *us;
  } ptype;
  SwigType     *type;
  String       *str;
  Parm         *p;
  ParmList     *pl;
  int           ivalue;
  Node         *node;
}
/* Line 187 of yacc.c.  */
#line 1967 "y.tab.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 1980 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  55
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   3828

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  127
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  146
/* YYNRULES -- Number of rules.  */
#define YYNRULES  464
/* YYNRULES -- Number of states.  */
#define YYNSTATES  900

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   381

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     9,    12,    16,    19,    25,    29,
      32,    34,    36,    38,    40,    42,    44,    46,    49,    51,
      53,    55,    57,    59,    61,    63,    65,    67,    69,    71,
      73,    75,    77,    79,    81,    83,    85,    87,    89,    91,
      92,   100,   106,   110,   116,   122,   126,   129,   132,   138,
     141,   147,   150,   155,   157,   159,   167,   175,   181,   182,
     190,   192,   194,   197,   200,   202,   208,   214,   220,   224,
     229,   233,   241,   250,   256,   260,   262,   264,   268,   270,
     275,   283,   290,   292,   294,   302,   312,   321,   332,   338,
     346,   353,   362,   364,   366,   372,   377,   383,   391,   393,
     397,   404,   411,   420,   422,   425,   429,   431,   434,   438,
     445,   451,   461,   464,   466,   468,   470,   471,   478,   484,
     486,   491,   493,   495,   498,   504,   511,   516,   524,   534,
     541,   543,   545,   547,   549,   551,   553,   554,   564,   565,
     575,   577,   581,   586,   587,   594,   598,   600,   602,   604,
     606,   608,   610,   612,   615,   617,   619,   621,   625,   627,
     631,   636,   637,   644,   645,   651,   657,   660,   661,   668,
     670,   672,   673,   677,   679,   681,   683,   685,   687,   689,
     691,   693,   697,   699,   701,   703,   705,   707,   709,   711,
     713,   715,   722,   729,   737,   746,   755,   763,   769,   772,
     775,   778,   779,   787,   788,   795,   797,   799,   801,   803,
     805,   807,   809,   811,   813,   815,   817,   820,   823,   826,
     831,   834,   840,   842,   845,   847,   849,   851,   853,   855,
     857,   859,   862,   864,   868,   870,   873,   881,   885,   887,
     890,   892,   896,   898,   900,   902,   905,   911,   914,   917,
     919,   922,   925,   927,   929,   931,   933,   936,   940,   942,
     945,   949,   954,   960,   965,   967,   970,   974,   979,   985,
     989,   994,   999,  1001,  1004,  1009,  1014,  1020,  1024,  1029,
    1034,  1036,  1039,  1042,  1046,  1048,  1051,  1053,  1056,  1060,
    1065,  1069,  1074,  1077,  1081,  1085,  1090,  1094,  1098,  1101,
    1104,  1106,  1108,  1111,  1113,  1115,  1117,  1119,  1122,  1124,
    1127,  1131,  1133,  1135,  1137,  1140,  1143,  1145,  1147,  1150,
    1152,  1154,  1157,  1159,  1161,  1163,  1165,  1167,  1169,  1171,
    1173,  1175,  1177,  1179,  1181,  1183,  1185,  1186,  1189,  1191,
    1193,  1197,  1199,  1201,  1205,  1207,  1209,  1211,  1213,  1215,
    1217,  1223,  1225,  1227,  1231,  1236,  1242,  1248,  1255,  1258,
    1261,  1263,  1265,  1267,  1269,  1271,  1273,  1275,  1277,  1281,
    1285,  1289,  1293,  1297,  1301,  1305,  1309,  1313,  1317,  1321,
    1325,  1329,  1333,  1337,  1341,  1347,  1350,  1353,  1356,  1359,
    1362,  1364,  1365,  1369,  1371,  1373,  1377,  1380,  1385,  1387,
    1389,  1391,  1393,  1395,  1397,  1399,  1401,  1403,  1405,  1407,
    1412,  1418,  1420,  1424,  1428,  1433,  1438,  1442,  1445,  1447,
    1449,  1453,  1456,  1460,  1462,  1464,  1466,  1468,  1470,  1473,
    1478,  1480,  1484,  1486,  1490,  1494,  1497,  1500,  1503,  1506,
    1509,  1514,  1516,  1520,  1522,  1526,  1530,  1533,  1536,  1539,
    1542,  1544,  1546,  1548,  1550,  1554,  1556,  1560,  1566,  1568,
    1572,  1576,  1582,  1584,  1586
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     128,     0,    -1,   129,    -1,   107,   213,    41,    -1,   107,
       1,    -1,   108,   213,    41,    -1,   108,     1,    -1,   109,
      38,   210,    39,    41,    -1,   109,     1,    41,    -1,   129,
     130,    -1,   272,    -1,   131,    -1,   168,    -1,   176,    -1,
      41,    -1,     1,    -1,   175,    -1,     1,   106,    -1,   132,
      -1,   134,    -1,   135,    -1,   136,    -1,   137,    -1,   138,
      -1,   141,    -1,   142,    -1,   145,    -1,   146,    -1,   147,
      -1,   148,    -1,   149,    -1,   150,    -1,   153,    -1,   155,
      -1,   158,    -1,   160,    -1,   165,    -1,   166,    -1,   167,
      -1,    -1,    62,   269,   262,    44,   133,   193,    45,    -1,
      86,   164,    44,   162,    45,    -1,    87,   162,    41,    -1,
      58,     3,    52,   235,    41,    -1,    58,   229,   221,   218,
      41,    -1,    58,     1,    41,    -1,    85,     4,    -1,    85,
     267,    -1,    84,    38,     3,    39,    44,    -1,    84,    44,
      -1,    84,    38,     3,    39,    41,    -1,    84,    41,    -1,
     267,    44,   213,    45,    -1,   267,    -1,   139,    -1,    89,
      38,   140,    40,   270,    39,     4,    -1,    89,    38,   140,
      40,   270,    39,    44,    -1,    89,    38,   140,    39,    41,
      -1,    -1,   144,   269,   267,    55,   143,   129,    56,    -1,
       7,    -1,     8,    -1,    82,     4,    -1,    82,    44,    -1,
       4,    -1,     9,    38,   260,    39,   267,    -1,     9,    38,
     260,    39,     4,    -1,     9,    38,   260,    39,    44,    -1,
      54,   269,   260,    -1,    59,    38,   260,    39,    -1,    59,
      38,    39,    -1,    81,    38,     3,    39,   209,     3,    41,
      -1,    81,    38,     3,    39,   209,   229,   221,    41,    -1,
      63,   152,     3,    52,   151,    -1,    63,   152,     3,    -1,
     267,    -1,     4,    -1,    38,     3,    39,    -1,   272,    -1,
     154,   221,   260,    41,    -1,   154,    38,   270,    39,   221,
     254,    41,    -1,   154,    38,   270,    39,   267,    41,    -1,
      60,    -1,    61,    -1,    64,    38,   260,    39,   221,   254,
     156,    -1,    64,    38,   260,    40,   271,    39,   221,   254,
      41,    -1,    64,    38,   260,   157,    39,   221,   254,   156,
      -1,    64,    38,   260,    40,   271,   157,    39,   221,   254,
      41,    -1,    64,    38,   260,    39,   156,    -1,    64,    38,
     260,    40,   271,    39,    41,    -1,    64,    38,   260,   157,
      39,   156,    -1,    64,    38,   260,    40,   271,   157,    39,
      41,    -1,   268,    -1,    41,    -1,   100,    38,   210,    39,
      41,    -1,    40,   260,    52,   271,    -1,    40,   260,    52,
     271,   157,    -1,    65,    38,   159,    39,   221,   254,    41,
      -1,   210,    -1,    11,    40,   213,    -1,    83,    38,   161,
      39,   162,   268,    -1,    83,    38,   161,    39,   162,    41,
      -1,    83,    38,   161,    39,   162,    52,   164,    41,    -1,
     270,    -1,   164,   163,    -1,    40,   164,   163,    -1,   272,
      -1,   229,   220,    -1,    38,   210,    39,    -1,    38,   210,
      39,    38,   210,    39,    -1,    99,    38,   210,    39,   156,
      -1,    88,    38,   261,    39,   265,    91,   214,    92,    41,
      -1,    90,   267,    -1,   170,    -1,   174,    -1,   173,    -1,
      -1,    42,   267,    44,   169,   129,    45,    -1,   209,   229,
     221,   172,   171,    -1,    41,    -1,    40,   221,   172,   171,
      -1,    44,    -1,   218,    -1,   227,   218,    -1,    76,    38,
     210,    39,   218,    -1,   227,    76,    38,   210,    39,   218,
      -1,   209,    66,     3,    41,    -1,   209,    66,   237,    44,
     238,    45,    41,    -1,   209,    66,   237,    44,   238,    45,
     221,   172,   171,    -1,   209,   229,    38,   210,    39,   255,
      -1,   177,    -1,   181,    -1,   182,    -1,   189,    -1,   190,
      -1,   200,    -1,    -1,   209,   252,   262,   245,    44,   178,
     193,    45,   180,    -1,    -1,   209,   252,    44,   179,   193,
      45,   221,   172,   171,    -1,    41,    -1,   221,   172,   171,
      -1,   209,   252,   262,    41,    -1,    -1,   104,    91,   185,
      92,   183,   184,    -1,   104,   252,   262,    -1,   170,    -1,
     177,    -1,   197,    -1,   182,    -1,   181,    -1,   199,    -1,
     186,    -1,   187,   188,    -1,   272,    -1,   251,    -1,   213,
      -1,    40,   187,   188,    -1,   272,    -1,    79,   262,    41,
      -1,    79,    80,   262,    41,    -1,    -1,    80,   262,    44,
     191,   129,    45,    -1,    -1,    80,    44,   192,   129,    45,
      -1,    80,     3,    52,   262,    41,    -1,   196,   193,    -1,
      -1,    62,    44,   194,   193,    45,   193,    -1,   142,    -1,
     272,    -1,    -1,     1,   195,   193,    -1,   168,    -1,   197,
      -1,   198,    -1,   201,    -1,   205,    -1,   199,    -1,   181,
      -1,   202,    -1,   209,   262,    41,    -1,   189,    -1,   182,
      -1,   200,    -1,   166,    -1,   167,    -1,   208,    -1,   141,
      -1,   165,    -1,    41,    -1,   209,   229,    38,   210,    39,
     255,    -1,   124,   264,    38,   210,    39,   206,    -1,    74,
     124,   264,    38,   210,    39,   207,    -1,   209,   106,   229,
     226,    38,   210,    39,   207,    -1,   209,   106,   229,   115,
      38,   210,    39,   207,    -1,   209,   106,   229,    38,   210,
      39,   207,    -1,    77,    38,   210,    39,    44,    -1,    70,
      72,    -1,    69,    72,    -1,    71,    72,    -1,    -1,   209,
     252,   262,   245,    44,   203,   180,    -1,    -1,   209,   252,
     245,    44,   204,   180,    -1,   150,    -1,   136,    -1,   148,
      -1,   153,    -1,   155,    -1,   158,    -1,   146,    -1,   160,
      -1,   134,    -1,   135,    -1,   137,    -1,   254,    41,    -1,
     254,    44,    -1,   254,    41,    -1,   254,    52,   235,    41,
      -1,   254,    44,    -1,   209,   229,    72,   241,    41,    -1,
      42,    -1,    42,   267,    -1,    73,    -1,    19,    -1,    74,
      -1,    75,    -1,    78,    -1,   272,    -1,   211,    -1,   213,
     212,    -1,   272,    -1,    40,   213,   212,    -1,   272,    -1,
     230,   219,    -1,   104,    91,   252,    92,   252,   262,   218,
      -1,    46,    46,    46,    -1,   215,    -1,   217,   216,    -1,
     272,    -1,    40,   217,   216,    -1,   272,    -1,   213,    -1,
     242,    -1,    52,   235,    -1,    52,   235,    55,   241,    56,
      -1,    52,    44,    -1,    72,   241,    -1,   272,    -1,   221,
     218,    -1,   224,   218,    -1,   218,    -1,   221,    -1,   224,
      -1,   272,    -1,   226,   222,    -1,   226,   115,   222,    -1,
     223,    -1,   115,   222,    -1,   262,   102,   222,    -1,   226,
     262,   102,   222,    -1,   226,   262,   102,   115,   222,    -1,
     262,   102,   115,   222,    -1,   262,    -1,   124,   262,    -1,
      38,   262,    39,    -1,    38,   226,   222,    39,    -1,    38,
     262,   102,   222,    39,    -1,   222,    55,    56,    -1,   222,
      55,   241,    56,    -1,   222,    38,   210,    39,    -1,   262,
      -1,   124,   262,    -1,    38,   226,   223,    39,    -1,    38,
     115,   223,    39,    -1,    38,   262,   102,   223,    39,    -1,
     223,    55,    56,    -1,   223,    55,   241,    56,    -1,   223,
      38,   210,    39,    -1,   226,    -1,   226,   225,    -1,   226,
     115,    -1,   226,   115,   225,    -1,   225,    -1,   115,   225,
      -1,   115,    -1,   262,   102,    -1,   226,   262,   102,    -1,
     226,   262,   102,   225,    -1,   225,    55,    56,    -1,   225,
      55,   241,    56,    -1,    55,    56,    -1,    55,   241,    56,
      -1,    38,   224,    39,    -1,   225,    38,   210,    39,    -1,
      38,   210,    39,    -1,   122,   227,   226,    -1,   122,   226,
      -1,   122,   227,    -1,   122,    -1,   228,    -1,   228,   227,
      -1,    47,    -1,    48,    -1,    49,    -1,   230,    -1,   227,
     231,    -1,   231,    -1,   231,   227,    -1,   227,   231,   227,
      -1,   232,    -1,    30,    -1,    28,    -1,    32,   259,    -1,
      66,   262,    -1,    33,    -1,   262,    -1,   252,   262,    -1,
     233,    -1,   234,    -1,   234,   233,    -1,    20,    -1,    22,
      -1,    23,    -1,    26,    -1,    27,    -1,    24,    -1,    25,
      -1,    29,    -1,    21,    -1,    31,    -1,    34,    -1,    35,
      -1,    36,    -1,    37,    -1,    -1,   236,   241,    -1,     3,
      -1,   272,    -1,   238,    40,   239,    -1,   239,    -1,     3,
      -1,     3,    52,   240,    -1,   272,    -1,   241,    -1,   242,
      -1,   229,    -1,   243,    -1,   267,    -1,    53,    38,   229,
     219,    39,    -1,   244,    -1,    10,    -1,    38,   241,    39,
      -1,    38,   241,    39,   241,    -1,    38,   241,   226,    39,
     241,    -1,    38,   241,   115,    39,   241,    -1,    38,   241,
     226,   115,    39,   241,    -1,   115,   241,    -1,   122,   241,
      -1,    11,    -1,    12,    -1,    13,    -1,    14,    -1,    15,
      -1,    16,    -1,    17,    -1,    18,    -1,   241,   119,   241,
      -1,   241,   118,   241,    -1,   241,   122,   241,    -1,   241,
     121,   241,    -1,   241,   120,   241,    -1,   241,   115,   241,
      -1,   241,   113,   241,    -1,   241,   114,   241,    -1,   241,
     117,   241,    -1,   241,   116,   241,    -1,   241,   112,   241,
      -1,   241,   111,   241,    -1,   241,    96,   241,    -1,   241,
      97,   241,    -1,   241,    95,   241,    -1,   241,    94,   241,
      -1,   241,    98,   241,    72,   241,    -1,   118,   241,    -1,
     119,   241,    -1,   124,   241,    -1,   123,   241,    -1,   229,
      38,    -1,   246,    -1,    -1,    72,   247,   248,    -1,   272,
      -1,   249,    -1,   248,    40,   249,    -1,   253,   262,    -1,
     253,   250,   253,   262,    -1,    70,    -1,    69,    -1,    71,
      -1,    67,    -1,    68,    -1,   251,    -1,    50,    -1,    51,
      -1,    74,    -1,   272,    -1,   227,    -1,    76,    38,   210,
      39,    -1,   227,    76,    38,   210,    39,    -1,   272,    -1,
     254,   256,    41,    -1,   254,   256,    44,    -1,    38,   210,
      39,    41,    -1,    38,   210,    39,    44,    -1,    52,   235,
      41,    -1,    72,   257,    -1,   272,    -1,   258,    -1,   257,
      40,   258,    -1,   262,    38,    -1,    91,   214,    92,    -1,
     272,    -1,     3,    -1,   267,    -1,   260,    -1,   272,    -1,
     264,   263,    -1,   101,   126,   264,   263,    -1,   264,    -1,
     101,   126,   264,    -1,   105,    -1,   101,   126,   105,    -1,
     126,   264,   263,    -1,   126,   264,    -1,   126,   105,    -1,
     103,   264,    -1,     3,   259,    -1,     3,   266,    -1,   101,
     126,     3,   266,    -1,     3,    -1,   101,   126,     3,    -1,
     105,    -1,   101,   126,   105,    -1,   126,     3,   266,    -1,
     126,     3,    -1,   126,   105,    -1,   103,     3,    -1,   267,
       6,    -1,     6,    -1,   267,    -1,    44,    -1,     4,    -1,
      38,   270,    39,    -1,   272,    -1,   260,    52,   271,    -1,
     260,    52,   271,    40,   270,    -1,   260,    -1,   260,    40,
     270,    -1,   260,    52,   139,    -1,   260,    52,   139,    40,
     270,    -1,   267,    -1,   243,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1745,  1745,  1758,  1762,  1765,  1768,  1771,  1774,  1779,
    1784,  1789,  1790,  1791,  1792,  1793,  1799,  1815,  1825,  1826,
    1827,  1828,  1829,  1830,  1831,  1832,  1833,  1834,  1835,  1836,
    1837,  1838,  1839,  1840,  1841,  1842,  1843,  1844,  1845,  1852,
    1852,  1924,  1934,  1945,  1966,  1988,  1999,  2008,  2027,  2033,
    2039,  2044,  2051,  2058,  2062,  2075,  2084,  2099,  2112,  2112,
    2167,  2168,  2175,  2195,  2226,  2230,  2240,  2245,  2263,  2303,
    2309,  2322,  2328,  2354,  2360,  2367,  2368,  2371,  2372,  2380,
    2426,  2472,  2483,  2486,  2513,  2519,  2525,  2531,  2539,  2545,
    2551,  2557,  2565,  2566,  2567,  2570,  2575,  2585,  2621,  2622,
    2652,  2669,  2677,  2690,  2715,  2721,  2725,  2728,  2739,  2744,
    2757,  2769,  3045,  3055,  3062,  3063,  3067,  3067,  3098,  3159,
    3163,  3185,  3191,  3197,  3203,  3209,  3222,  3237,  3247,  3325,
    3376,  3377,  3378,  3379,  3380,  3381,  3386,  3386,  3629,  3629,
    3752,  3753,  3765,  3785,  3785,  4074,  4080,  4083,  4086,  4089,
    4092,  4095,  4100,  4130,  4134,  4137,  4140,  4145,  4149,  4154,
    4164,  4195,  4195,  4224,  4224,  4246,  4273,  4288,  4288,  4298,
    4299,  4300,  4300,  4316,  4317,  4334,  4335,  4336,  4337,  4338,
    4339,  4340,  4341,  4342,  4343,  4344,  4345,  4346,  4347,  4348,
    4349,  4358,  4383,  4407,  4448,  4463,  4481,  4500,  4507,  4514,
    4522,  4543,  4543,  4568,  4568,  4603,  4606,  4610,  4613,  4614,
    4615,  4616,  4617,  4618,  4619,  4620,  4623,  4628,  4635,  4643,
    4651,  4662,  4668,  4669,  4677,  4678,  4679,  4680,  4681,  4682,
    4689,  4700,  4704,  4707,  4711,  4715,  4725,  4733,  4741,  4754,
    4758,  4761,  4765,  4769,  4797,  4805,  4816,  4830,  4839,  4847,
    4857,  4861,  4865,  4872,  4889,  4906,  4914,  4922,  4931,  4935,
    4944,  4955,  4967,  4977,  4990,  4997,  5005,  5021,  5029,  5040,
    5051,  5062,  5081,  5089,  5106,  5114,  5121,  5132,  5143,  5154,
    5173,  5179,  5185,  5192,  5201,  5204,  5213,  5220,  5227,  5237,
    5248,  5259,  5270,  5277,  5284,  5287,  5304,  5314,  5321,  5327,
    5332,  5338,  5342,  5348,  5349,  5350,  5356,  5362,  5366,  5367,
    5371,  5378,  5381,  5382,  5383,  5384,  5385,  5387,  5390,  5395,
    5420,  5423,  5477,  5481,  5485,  5489,  5493,  5497,  5501,  5505,
    5509,  5513,  5517,  5521,  5525,  5529,  5535,  5535,  5561,  5562,
    5565,  5578,  5586,  5594,  5611,  5614,  5629,  5630,  5649,  5650,
    5654,  5659,  5660,  5674,  5681,  5698,  5705,  5712,  5720,  5724,
    5730,  5731,  5732,  5733,  5734,  5735,  5736,  5737,  5740,  5744,
    5748,  5752,  5756,  5760,  5764,  5768,  5772,  5776,  5780,  5784,
    5788,  5792,  5806,  5810,  5814,  5820,  5824,  5828,  5832,  5836,
    5852,  5857,  5857,  5858,  5861,  5878,  5887,  5900,  5913,  5914,
    5915,  5919,  5923,  5929,  5932,  5936,  5942,  5943,  5946,  5951,
    5956,  5961,  5968,  5975,  5982,  5990,  5998,  6006,  6007,  6010,
    6011,  6014,  6020,  6026,  6029,  6030,  6033,  6034,  6037,  6042,
    6046,  6049,  6052,  6055,  6060,  6064,  6067,  6074,  6080,  6089,
    6094,  6098,  6101,  6104,  6107,  6112,  6116,  6119,  6122,  6128,
    6133,  6136,  6139,  6143,  6148,  6161,  6165,  6170,  6176,  6180,
    6185,  6189,  6196,  6199,  6204
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ID", "HBLOCK", "POUND", "STRING",
  "INCLUDE", "IMPORT", "INSERT", "CHARCONST", "NUM_INT", "NUM_FLOAT",
  "NUM_UNSIGNED", "NUM_LONG", "NUM_ULONG", "NUM_LONGLONG", "NUM_ULONGLONG",
  "NUM_BOOL", "TYPEDEF", "TYPE_INT", "TYPE_UNSIGNED", "TYPE_SHORT",
  "TYPE_LONG", "TYPE_FLOAT", "TYPE_DOUBLE", "TYPE_CHAR", "TYPE_WCHAR",
  "TYPE_VOID", "TYPE_SIGNED", "TYPE_BOOL", "TYPE_COMPLEX", "TYPE_TYPEDEF",
  "TYPE_RAW", "TYPE_NON_ISO_INT8", "TYPE_NON_ISO_INT16",
  "TYPE_NON_ISO_INT32", "TYPE_NON_ISO_INT64", "LPAREN", "RPAREN", "COMMA",
  "SEMI", "EXTERN", "INIT", "LBRACE", "RBRACE", "PERIOD", "CONST_QUAL",
  "VOLATILE", "REGISTER", "STRUCT", "UNION", "EQUAL", "SIZEOF", "MODULE",
  "LBRACKET", "RBRACKET", "ILLEGAL", "CONSTANT", "NAME", "RENAME",
  "NAMEWARN", "EXTEND", "PRAGMA", "FEATURE", "VARARGS", "ENUM", "CLASS",
  "TYPENAME", "PRIVATE", "PUBLIC", "PROTECTED", "COLON", "STATIC",
  "VIRTUAL", "FRIEND", "THROW", "CATCH", "EXPLICIT", "USING", "NAMESPACE",
  "NATIVE", "INLINE", "TYPEMAP", "EXCEPT", "ECHO", "APPLY", "CLEAR",
  "SWIGTEMPLATE", "FRAGMENT", "WARN", "LESSTHAN", "GREATERTHAN",
  "DELETE_KW", "LESSTHANOREQUALTO", "GREATERTHANOREQUALTO", "EQUALTO",
  "NOTEQUALTO", "QUESTIONMARK", "TYPES", "PARMS", "NONID", "DSTAR",
  "DCNOT", "TEMPLATE", "OPERATOR", "COPERATOR", "PARSETYPE", "PARSEPARM",
  "PARSEPARMS", "CAST", "LOR", "LAND", "OR", "XOR", "AND", "RSHIFT",
  "LSHIFT", "MINUS", "PLUS", "MODULO", "SLASH", "STAR", "LNOT", "NOT",
  "UMINUS", "DCOLON", "$accept", "program", "interface", "declaration",
  "swig_directive", "extend_directive", "@1", "apply_directive",
  "clear_directive", "constant_directive", "echo_directive",
  "except_directive", "stringtype", "fname", "fragment_directive",
  "include_directive", "@2", "includetype", "inline_directive",
  "insert_directive", "module_directive", "name_directive",
  "native_directive", "pragma_directive", "pragma_arg", "pragma_lang",
  "rename_directive", "rename_namewarn", "feature_directive",
  "stringbracesemi", "featattr", "varargs_directive", "varargs_parms",
  "typemap_directive", "typemap_type", "tm_list", "tm_tail",
  "typemap_parm", "types_directive", "template_directive",
  "warn_directive", "c_declaration", "@3", "c_decl", "c_decl_tail",
  "initializer", "c_enum_forward_decl", "c_enum_decl",
  "c_constructor_decl", "cpp_declaration", "cpp_class_decl", "@4", "@5",
  "cpp_opt_declarators", "cpp_forward_class_decl", "cpp_template_decl",
  "@6", "cpp_temp_possible", "template_parms", "templateparameters",
  "templateparameter", "templateparameterstail", "cpp_using_decl",
  "cpp_namespace_decl", "@7", "@8", "cpp_members", "@9", "@10",
  "cpp_member", "cpp_constructor_decl", "cpp_destructor_decl",
  "cpp_conversion_operator", "cpp_catch_decl", "cpp_protection_decl",
  "cpp_nested", "@11", "@12", "cpp_swig_directive", "cpp_end", "cpp_vend",
  "anonymous_bitfield", "storage_class", "parms", "rawparms", "ptail",
  "parm", "valparms", "rawvalparms", "valptail", "valparm", "def_args",
  "parameter_declarator", "typemap_parameter_declarator", "declarator",
  "notso_direct_declarator", "direct_declarator", "abstract_declarator",
  "direct_abstract_declarator", "pointer", "type_qualifier",
  "type_qualifier_raw", "type", "rawtype", "type_right", "primitive_type",
  "primitive_type_list", "type_specifier", "definetype", "@13", "ename",
  "enumlist", "edecl", "etype", "expr", "valexpr", "exprnum",
  "exprcompound", "inherit", "raw_inherit", "@14", "base_list",
  "base_specifier", "access_specifier", "templcpptype", "cpptype",
  "opt_virtual", "cpp_const", "ctor_end", "ctor_initializer",
  "mem_initializer_list", "mem_initializer", "template_decl", "idstring",
  "idstringopt", "idcolon", "idcolontail", "idtemplate", "idcolonnt",
  "idcolontailnt", "string", "stringbrace", "options", "kwargs",
  "stringnum", "empty", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   127,   128,   128,   128,   128,   128,   128,   128,   129,
     129,   130,   130,   130,   130,   130,   130,   130,   131,   131,
     131,   131,   131,   131,   131,   131,   131,   131,   131,   131,
     131,   131,   131,   131,   131,   131,   131,   131,   131,   133,
     132,   134,   135,   136,   136,   136,   137,   137,   138,   138,
     138,   138,   139,   140,   140,   141,   141,   141,   143,   142,
     144,   144,   145,   145,   146,   146,   146,   146,   147,   148,
     148,   149,   149,   150,   150,   151,   151,   152,   152,   153,
     153,   153,   154,   154,   155,   155,   155,   155,   155,   155,
     155,   155,   156,   156,   156,   157,   157,   158,   159,   159,
     160,   160,   160,   161,   162,   163,   163,   164,   164,   164,
     165,   166,   167,   168,   168,   168,   169,   168,   170,   171,
     171,   171,   172,   172,   172,   172,   173,   174,   174,   175,
     176,   176,   176,   176,   176,   176,   178,   177,   179,   177,
     180,   180,   181,   183,   182,   182,   184,   184,   184,   184,
     184,   184,   185,   186,   186,   187,   187,   188,   188,   189,
     189,   191,   190,   192,   190,   190,   193,   194,   193,   193,
     193,   195,   193,   196,   196,   196,   196,   196,   196,   196,
     196,   196,   196,   196,   196,   196,   196,   196,   196,   196,
     196,   197,   198,   198,   199,   199,   199,   200,   201,   201,
     201,   203,   202,   204,   202,   205,   205,   205,   205,   205,
     205,   205,   205,   205,   205,   205,   206,   206,   207,   207,
     207,   208,   209,   209,   209,   209,   209,   209,   209,   209,
     210,   211,   211,   212,   212,   213,   213,   213,   214,   215,
     215,   216,   216,   217,   217,   218,   218,   218,   218,   218,
     219,   219,   219,   220,   220,   220,   221,   221,   221,   221,
     221,   221,   221,   221,   222,   222,   222,   222,   222,   222,
     222,   222,   223,   223,   223,   223,   223,   223,   223,   223,
     224,   224,   224,   224,   224,   224,   224,   224,   224,   224,
     225,   225,   225,   225,   225,   225,   225,   226,   226,   226,
     226,   227,   227,   228,   228,   228,   229,   230,   230,   230,
     230,   231,   231,   231,   231,   231,   231,   231,   231,   232,
     233,   233,   234,   234,   234,   234,   234,   234,   234,   234,
     234,   234,   234,   234,   234,   234,   236,   235,   237,   237,
     238,   238,   239,   239,   239,   240,   241,   241,   242,   242,
     242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     243,   243,   243,   243,   243,   243,   243,   243,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     245,   247,   246,   246,   248,   248,   249,   249,   250,   250,
     250,   251,   251,   252,   252,   252,   253,   253,   254,   254,
     254,   254,   255,   255,   255,   255,   255,   256,   256,   257,
     257,   258,   259,   259,   260,   260,   261,   261,   262,   262,
     262,   262,   262,   262,   263,   263,   263,   263,   264,   265,
     265,   265,   265,   265,   265,   266,   266,   266,   266,   267,
     267,   268,   268,   268,   269,   269,   270,   270,   270,   270,
     270,   270,   271,   271,   272
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     3,     2,     3,     2,     5,     3,     2,
       1,     1,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       7,     5,     3,     5,     5,     3,     2,     2,     5,     2,
       5,     2,     4,     1,     1,     7,     7,     5,     0,     7,
       1,     1,     2,     2,     1,     5,     5,     5,     3,     4,
       3,     7,     8,     5,     3,     1,     1,     3,     1,     4,
       7,     6,     1,     1,     7,     9,     8,    10,     5,     7,
       6,     8,     1,     1,     5,     4,     5,     7,     1,     3,
       6,     6,     8,     1,     2,     3,     1,     2,     3,     6,
       5,     9,     2,     1,     1,     1,     0,     6,     5,     1,
       4,     1,     1,     2,     5,     6,     4,     7,     9,     6,
       1,     1,     1,     1,     1,     1,     0,     9,     0,     9,
       1,     3,     4,     0,     6,     3,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     3,     1,     3,
       4,     0,     6,     0,     5,     5,     2,     0,     6,     1,
       1,     0,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     6,     6,     7,     8,     8,     7,     5,     2,     2,
       2,     0,     7,     0,     6,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     2,     2,     4,
       2,     5,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     3,     1,     2,     7,     3,     1,     2,
       1,     3,     1,     1,     1,     2,     5,     2,     2,     1,
       2,     2,     1,     1,     1,     1,     2,     3,     1,     2,
       3,     4,     5,     4,     1,     2,     3,     4,     5,     3,
       4,     4,     1,     2,     4,     4,     5,     3,     4,     4,
       1,     2,     2,     3,     1,     2,     1,     2,     3,     4,
       3,     4,     2,     3,     3,     4,     3,     3,     2,     2,
       1,     1,     2,     1,     1,     1,     1,     2,     1,     2,
       3,     1,     1,     1,     2,     2,     1,     1,     2,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     2,     1,     1,
       3,     1,     1,     3,     1,     1,     1,     1,     1,     1,
       5,     1,     1,     3,     4,     5,     5,     6,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     5,     2,     2,     2,     2,     2,
       1,     0,     3,     1,     1,     3,     2,     4,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       5,     1,     3,     3,     4,     4,     3,     2,     1,     1,
       3,     2,     3,     1,     1,     1,     1,     1,     2,     4,
       1,     3,     1,     3,     3,     2,     2,     2,     2,     2,
       4,     1,     3,     1,     3,     3,     2,     2,     2,     2,
       1,     1,     1,     1,     3,     1,     3,     5,     1,     3,
       3,     5,     1,     1,     0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
     464,     0,     0,     0,     0,     0,    10,     4,   464,   322,
     330,   323,   324,   327,   328,   325,   326,   313,   329,   312,
     331,   464,   316,   332,   333,   334,   335,     0,   303,   304,
     305,   404,   405,     0,   401,   402,     0,     0,   432,     0,
       0,   301,   464,   308,   311,   319,   320,   403,     0,   317,
     430,     6,     0,     0,   464,     1,    15,    64,    60,    61,
       0,   225,    14,   222,   464,     0,     0,    82,    83,   464,
     464,     0,     0,   224,   226,   227,     0,   228,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     9,    11,    18,    19,    20,    21,    22,    23,
      24,    25,   464,    26,    27,    28,    29,    30,    31,    32,
       0,    33,    34,    35,    36,    37,    38,    12,   113,   115,
     114,    16,    13,   130,   131,   132,   133,   134,   135,     0,
     229,   464,   438,   423,   314,     0,   315,     0,     0,     3,
     307,   302,   464,   336,     0,     0,   286,   300,     0,   252,
     235,   464,   258,   464,   284,   280,   272,   249,   309,   321,
     318,     0,     0,   428,     5,     8,     0,   230,   464,   232,
      17,     0,   450,   223,     0,     0,   455,     0,   464,     0,
     306,     0,     0,     0,     0,    78,     0,   464,   464,     0,
       0,   464,   163,     0,     0,    62,    63,     0,     0,    51,
      49,    46,    47,   464,     0,   464,     0,   464,   464,     0,
     112,   464,   464,     0,     0,     0,     0,     0,     0,   272,
     464,     0,     0,   352,   360,   361,   362,   363,   364,   365,
     366,   367,     0,     0,     0,     0,     0,     0,     0,     0,
     243,     0,   238,   464,   347,   306,     0,   346,   348,   351,
     349,   240,   237,   433,   431,     0,   310,   464,   286,     0,
       0,   280,   317,   247,   245,     0,   292,     0,   346,   248,
     464,     0,   259,   285,   264,   298,   299,   273,   250,   464,
       0,   251,   464,     0,   282,   256,   281,   264,   287,   437,
     436,   435,     0,     0,   231,   234,   424,     0,   425,   449,
     116,   458,     0,    68,    45,   336,     0,   464,    70,     0,
       0,     0,    74,     0,     0,     0,    98,     0,     0,   159,
       0,   464,   161,     0,     0,   103,     0,     0,     0,   107,
     253,   254,   255,    42,     0,   104,   106,   426,     0,   427,
      54,     0,    53,     0,     0,   152,   464,   156,   403,   154,
     145,     0,   424,     0,     0,     0,     0,     0,     0,     0,
     264,     0,   464,     0,   339,   464,   464,   138,   318,     0,
       0,   358,   385,   386,   359,   388,   387,   422,     0,   239,
     242,   389,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   429,
       0,   286,   280,   317,     0,   272,   296,   294,   282,     0,
     272,   287,     0,   337,   293,   280,   317,   265,   464,     0,
     297,     0,   277,     0,     0,   290,     0,   257,   283,   288,
       0,   260,   434,     7,   464,     0,   464,     0,     0,   454,
       0,     0,    69,    39,    77,     0,     0,     0,     0,     0,
       0,     0,   160,     0,     0,   464,   464,     0,     0,   108,
       0,   464,     0,     0,     0,     0,     0,   143,     0,   153,
     158,    58,     0,     0,     0,     0,    79,     0,   126,   464,
       0,   317,     0,     0,   122,   464,     0,   142,   391,     0,
     390,   393,   353,     0,   300,     0,   464,   464,   383,   382,
     380,   381,     0,   379,   378,   374,   375,   373,   377,   376,
     369,   368,   372,   371,   370,     0,     0,   287,   275,   274,
     288,     0,     0,     0,   264,   266,   287,     0,   269,     0,
     279,   278,   295,   291,     0,   261,   289,   263,   233,    66,
      67,    65,     0,   459,   460,   463,   462,   456,    43,    44,
       0,    76,    73,    75,   453,    93,   452,     0,    88,   464,
     451,    92,     0,   462,     0,     0,    99,   464,   197,   165,
     164,     0,   222,     0,     0,    50,    48,   464,    41,   105,
     441,     0,   443,     0,    57,     0,     0,   110,   464,   464,
     464,   464,     0,     0,   342,     0,   341,   344,   464,   464,
       0,   119,   121,   118,     0,   123,   171,   190,     0,     0,
       0,     0,   226,     0,   213,   214,   206,   215,   188,   169,
     211,   207,   205,   208,   209,   210,   212,   189,   185,   186,
     173,   179,   183,   182,     0,     0,   174,   175,   178,   184,
     176,   180,   177,   187,     0,   229,   464,   136,   354,     0,
     300,   299,     0,     0,     0,   241,     0,   464,   276,   246,
     267,     0,   271,   270,   262,   117,     0,     0,     0,   464,
       0,   408,     0,   411,     0,     0,     0,     0,    90,   464,
       0,   162,   223,   464,     0,   101,     0,   100,     0,     0,
       0,   439,     0,   464,     0,    52,   146,   147,   150,   149,
     144,   148,   151,     0,   157,     0,     0,    81,     0,   464,
       0,   464,   336,   464,   129,     0,   464,   464,     0,   167,
     199,   198,   200,     0,     0,     0,   166,     0,     0,   464,
     317,   406,   392,   394,     0,   407,     0,   356,   355,     0,
     350,   384,   236,   268,   461,   457,    40,     0,   464,     0,
      84,   462,    95,    89,   464,     0,     0,    97,    71,     0,
       0,   109,   448,   446,   447,   442,   444,     0,    55,    56,
       0,    59,    80,   343,   345,   340,   127,   464,     0,     0,
       0,     0,   418,   464,     0,     0,   172,     0,     0,   464,
     464,     0,   464,     0,     0,   318,   181,   464,   399,   398,
     400,   464,   396,     0,   357,     0,     0,   464,    96,     0,
      91,   464,    86,    72,   102,   445,   440,     0,     0,     0,
     416,   417,   419,     0,   412,   413,   124,   120,   464,     0,
     464,     0,     0,   464,     0,     0,     0,     0,   203,     0,
     395,     0,     0,    94,   409,     0,    85,     0,   111,   128,
     414,   415,     0,   421,   125,     0,     0,   464,   139,     0,
     464,   464,   464,   221,     0,   201,   397,   140,   137,   464,
     410,    87,   420,   168,   464,   192,     0,   464,     0,     0,
     191,   204,     0,     0,   193,     0,   216,   217,   196,   464,
     464,   202,   141,   218,   220,   336,   195,   194,     0,   219
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,     5,    92,    93,    94,   550,   614,   615,   616,
     617,    99,   340,   341,   618,   619,   590,   102,   103,   620,
     105,   621,   107,   622,   552,   184,   623,   110,   624,   558,
     448,   625,   315,   626,   324,   206,   335,   207,   627,   628,
     629,   630,   436,   118,   603,   483,   119,   120,   121,   122,
     123,   736,   486,   868,   631,   632,   588,   700,   344,   345,
     346,   469,   633,   127,   455,   321,   634,   787,   718,   635,
     636,   637,   638,   639,   640,   641,   882,   864,   642,   875,
     884,   643,   644,   259,   167,   294,   168,   241,   242,   379,
     243,   484,   150,   329,   151,   272,   152,   153,   154,   218,
      40,    41,   244,   180,    43,    44,    45,    46,   264,   265,
     363,   595,   596,   773,   246,   268,   248,   249,   489,   490,
     646,   732,   733,   801,    47,    48,   734,   885,   714,   781,
     821,   822,   132,   301,   338,    49,   163,    50,   583,   691,
     250,   561,   175,   302,   547,   169
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -829
static const yytype_int16 yypact[] =
{
     599,  3107,  3167,    33,    51,  2621,  -829,  -829,   -25,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,   -25,  -829,  -829,  -829,  -829,  -829,   117,  -829,  -829,
    -829,  -829,  -829,    85,  -829,  -829,   -43,    98,  -829,   197,
    3709,   769,   306,   769,  -829,  -829,  2576,  -829,    85,  -829,
     187,  -829,   277,   290,  3444,  -829,    29,  -829,  -829,  -829,
     133,  -829,  -829,   341,   325,  3226,   347,  -829,  -829,   325,
     351,   389,   400,  -829,  -829,  -829,   421,  -829,    35,   130,
     427,   120,   438,   428,   176,  3493,  3493,   440,   445,   341,
     450,   455,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,   325,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
     338,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  3552,
    -829,  1537,  -829,  -829,  -829,   447,  -829,    15,   512,  -829,
     769,  -829,  2337,   451,  1682,  2229,    94,   287,    85,  -829,
    -829,   332,    17,   332,   166,   403,   405,  -829,  -829,  -829,
    -829,   529,    56,  -829,  -829,  -829,   495,  -829,   504,  -829,
    -829,   336,  -829,   260,   336,   336,  -829,   510,    46,   544,
    -829,   211,    85,   555,   570,  -829,   336,  3384,  3444,    85,
     536,   164,  -829,   551,   593,  -829,  -829,   336,   617,  -829,
    -829,  -829,   603,  3444,   577,    87,   591,   602,   336,   341,
     603,  3444,  3444,    85,   341,   266,   225,   336,  1070,   537,
     198,   820,   169,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,  2229,   609,  2229,  2229,  2229,  2229,  2229,  2229,
    -829,   575,  -829,   629,   638,   268,  1779,    16,  -829,  -829,
     603,  -829,  -829,  -829,   187,   582,  -829,  2398,   118,   641,
     645,  1010,   584,  -829,   622,  2229,  -829,  1645,  -829,  1779,
    2398,    85,   229,   166,  -829,  -829,   566,  -829,  -829,  3444,
    1804,  -829,  3444,  1926,    94,   229,   166,   587,  1135,  -829,
    -829,   187,   658,  3444,  -829,  -829,  -829,   655,   603,  -829,
    -829,   372,   684,  -829,  -829,  -829,   286,   332,  -829,   685,
     682,   693,   690,   354,   694,   698,  -829,   706,   705,  -829,
      85,  -829,  -829,   712,   716,  -829,   718,   719,  3493,  -829,
    -829,  -829,  -829,  -829,  3493,  -829,  -829,  -829,   722,  -829,
    -829,   460,   267,   725,   656,  -829,   739,  -829,    18,  -829,
    -829,   111,   349,   295,   295,   678,   753,    76,   755,   225,
     696,  1135,    32,   761,  -829,  2458,   606,  -829,   220,  1292,
    3601,  1674,  -829,  -829,  -829,  -829,  -829,  -829,  1537,  -829,
    -829,  -829,  2229,  2229,  2229,  2229,  2229,  2229,  2229,  2229,
    2229,  2229,  2229,  2229,  2229,  2229,  2229,  2229,  2229,  -829,
     512,   387,   124,   697,   315,  -829,  -829,  -829,   387,   378,
     710,   295,  2229,  1779,  -829,  1059,    -2,  -829,  3444,  2048,
    -829,   774,  -829,  1767,   786,  -829,  1889,   229,   166,  1064,
     225,   229,  -829,  -829,   504,   190,  -829,   336,  1336,  -829,
     797,   799,  -829,  -829,  -829,   271,  1017,   974,   788,  3444,
     544,   798,  -829,   805,  2720,  -829,   790,  3493,   406,   811,
     808,   602,   200,   809,   336,  3444,   275,  -829,  3444,  -829,
    -829,  -829,   295,   845,   225,     5,  -829,  1146,  -829,   853,
     821,   678,   828,   288,  -829,   415,  1422,  -829,  -829,   826,
    -829,  -829,  2229,  2107,  2288,   -19,   306,   629,  1239,  1239,
    1170,  1170,  3675,  1199,  2082,  1109,  1505,  1674,   653,   653,
     700,   700,  -829,  -829,  -829,    85,   710,  -829,  -829,  -829,
     387,   393,  2473,   481,   710,  -829,   225,   834,  -829,  3706,
    -829,  -829,  -829,  -829,   225,   229,   166,   229,  -829,  -829,
    -829,   603,  2819,  -829,   836,  -829,   267,   839,  -829,  -829,
    1422,  -829,  -829,   603,  -829,  -829,  -829,   842,  -829,   350,
     603,  -829,   830,    37,   500,  1017,  -829,   350,  -829,  -829,
    -829,  2918,   341,  3660,   621,  -829,  -829,  3444,  -829,  -829,
     219,   759,  -829,   795,  -829,   848,   843,  -829,   340,   739,
    -829,   350,   241,   225,   846,    73,  -829,  -829,   875,  3444,
     544,  -829,  -829,  -829,   856,  -829,  -829,  -829,   847,   823,
     827,   829,   776,   529,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,  -829,  -829,   857,  1422,  -829,  -829,  -829,  -829,
    -829,  -829,  -829,  -829,  3275,   859,   833,  -829,  1779,  2229,
    2288,  2515,  2229,   869,   871,  -829,  2229,   332,  -829,  -829,
    -829,   490,  -829,  -829,   229,  -829,   336,   336,   870,  3444,
     876,   844,   275,  -829,  1336,   250,   336,   879,  -829,   350,
     888,  -829,   603,   -11,   544,  -829,  3493,  -829,   891,   928,
      78,  -829,   147,  1537,   180,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,  -829,  3335,  -829,  3017,   893,  -829,  2229,   853,
     878,  3444,  -829,   864,  -829,   898,   606,  3444,  1422,  -829,
    -829,  -829,  -829,   529,   905,   544,  -829,  3601,   709,   125,
     904,  -829,   908,  -829,   432,  -829,  1422,  1779,  1779,  2229,
    -829,  1901,  -829,  -829,  -829,  -829,  -829,   910,  3444,   915,
    -829,   603,   921,  -829,   350,   935,   275,  -829,  -829,   922,
     924,  -829,  -829,   219,  -829,   219,  -829,   874,  -829,  -829,
    1067,  -829,  -829,  -829,  1779,  -829,  -829,   606,   929,   931,
      85,   470,  -829,   332,   288,   936,  -829,  1422,   944,  3444,
     606,    19,  2458,  2229,   930,   220,  -829,   833,  -829,  -829,
    -829,   833,  -829,   950,  1779,   955,   959,  3444,  -829,   960,
    -829,   350,  -829,  -829,  -829,  -829,  -829,   962,   288,   525,
    -829,   964,  -829,   967,  -829,  -829,  -829,  -829,   332,   965,
    3444,   970,   288,  3444,   973,   976,   978,  1551,  -829,   968,
    -829,    85,  1028,  -829,  -829,   979,  -829,   985,  -829,  -829,
    -829,  -829,    85,  -829,  -829,  1422,   980,   350,  -829,   988,
    3444,  3444,   875,  -829,  1028,  -829,  -829,  -829,  -829,   606,
    -829,  -829,  -829,  -829,   350,  -829,   543,   350,   990,   991,
    -829,  -829,  1028,   288,  -829,   497,  -829,  -829,  -829,   350,
     350,  -829,  -829,  -829,  -829,  -829,  -829,  -829,   998,  -829
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -829,  -829,  -307,  -829,  -829,  -829,  -829,    10,    30,    36,
      44,  -829,   586,  -829,    55,    58,  -829,  -829,  -829,    62,
    -829,    63,  -829,    77,  -829,  -829,    81,  -829,    89,  -462,
    -552,    99,  -829,   101,  -829,  -312,   554,   -72,   114,   121,
     139,   149,  -829,   444,  -723,  -699,  -829,  -829,  -829,  -829,
     453,  -829,  -829,  -828,    23,    28,  -829,  -829,  -829,  -829,
     574,   454,   154,  -829,  -829,  -829,  -543,  -829,  -829,  -829,
     457,  -829,   458,   236,  -829,  -829,  -829,  -829,  -829,  -829,
    -239,  -829,    14,   192,  -829,   620,    24,   358,  -829,   559,
     686,   -39,   567,  -829,    75,   674,  -196,  -118,  -115,    80,
     217,  -829,   636,    45,   -34,  -829,  1022,  -829,  -283,  -829,
    -829,  -829,   367,  -829,   983,  -129,  -415,  -829,  -702,  -829,
    -829,  -829,   284,  -829,  -203,   -90,   282,  -514,   222,  -829,
    -829,   233,  1066,   -70,  -829,   482,  -124,   -87,  -829,  -343,
     863,   514,   -17,  -186,  -437,     0
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -465
static const yytype_int16 yytable[] =
{
       6,   213,   247,   149,   587,   130,   140,   668,   133,   348,
     564,   325,   677,   204,   454,    95,   460,   784,     8,   129,
     652,   133,   440,   545,   260,    39,    52,   794,   124,   356,
     758,   273,   545,   125,    53,    96,   881,   525,     8,   222,
     286,    97,   157,   299,   525,   672,    42,    42,   255,    98,
     254,    55,   182,   680,   891,   279,  -244,   833,  -155,     8,
     100,   827,   404,   101,   176,   409,   131,   104,   106,   176,
     185,    54,   280,   478,   289,   291,  -338,   706,   818,     8,
     131,   763,   108,   137,   713,   214,   109,   331,     8,  -425,
       8,   832,   726,   839,   111,   849,   653,     8,   305,    42,
     526,   297,   176,   678,   112,   303,   113,   593,  -244,   858,
    -155,   309,   278,   709,   281,   189,   313,   299,   710,   114,
     253,     8,   155,   131,   195,   142,   115,     8,     8,   542,
     399,   251,   270,   191,   834,   170,    36,   131,   337,   260,
      38,   147,   144,   273,   116,   574,   286,   358,   571,   144,
     765,   157,   260,   157,   117,   240,   142,   404,   409,   126,
     892,   290,   257,   135,   196,   756,   471,   432,   295,   428,
     883,   171,     8,   144,   192,   786,   245,    36,   133,   144,
     201,    38,   172,   764,   768,   217,    36,    42,    36,   138,
      38,   133,    38,   803,   539,    36,   172,   488,   147,    38,
     808,   362,   146,   580,   282,   332,   149,   336,   339,   147,
     750,   148,   349,   367,   296,   521,   320,   172,   271,    36,
     364,   283,   261,    38,   769,    36,    36,   275,     8,    38,
      38,    36,    42,    42,   540,    38,   347,   752,   139,   408,
     809,   128,   148,   380,   829,   157,   166,   299,    42,   247,
     308,   543,   766,     8,   307,   131,    42,    42,   141,   545,
     158,   487,   461,   357,  -464,   348,   299,   418,   441,   352,
      36,     8,   172,   299,    38,   551,   521,   172,   585,   554,
     330,   172,   707,   705,   419,   155,   273,   286,   306,     8,
     161,   753,   488,   428,   812,   354,   366,   847,     8,    36,
     286,   581,    42,    38,   300,   582,   142,   157,  -464,     8,
     515,   465,   873,   162,   536,    42,   555,   434,   164,   556,
     143,     6,   689,   144,    42,   155,    36,    42,   600,   601,
      38,   165,   602,   306,    28,    29,    30,   402,    42,   296,
     145,     8,   172,   876,   142,   690,   470,   172,   713,   271,
     415,    36,   133,   279,   518,    38,   420,   256,   143,    61,
    -464,   144,   133,   174,   276,   216,   157,    36,   491,    36,
     280,    38,   147,    38,   148,   557,   215,   562,   145,   316,
     317,   353,   572,   146,   143,   181,   354,    36,   147,   183,
     147,    38,   148,   446,   447,   327,    36,    28,    29,    30,
      38,   353,   240,   343,   145,   536,     8,    36,   147,   147,
      42,    38,   437,    73,    74,    75,   279,   519,    77,   148,
     815,   146,   816,   245,   438,   257,   670,   186,   147,   779,
     148,   279,   658,   280,   295,     8,     6,   474,   187,    36,
     131,   270,   144,    38,    91,   354,   605,   575,   280,   495,
     576,  -464,  -464,   216,   130,     6,   130,   149,   144,   188,
     147,   336,   148,    42,    95,   194,   198,   143,   129,   199,
     573,   421,   200,   566,   424,  -464,   197,   124,   208,   597,
     744,   745,   125,   209,    96,   157,   645,   145,   211,   586,
      97,   604,   347,   252,    42,   263,   157,   380,    98,   463,
     464,   798,   799,   800,    36,    31,    32,   288,    38,   100,
      42,   824,   101,    42,   825,   136,   104,   106,   284,   418,
     660,   559,    34,    35,   156,   567,   724,   271,   418,   743,
     160,   108,     8,    36,   292,   109,   419,    38,   893,   675,
     676,   894,   130,   111,   293,   419,   212,     8,   591,   895,
     645,   304,    95,   112,   729,   113,   129,   480,   311,   673,
     190,   193,    31,    32,   247,   124,   850,   673,   114,   851,
     125,   130,    96,   312,   275,   115,   155,   319,    97,    34,
      35,    95,   306,   485,   886,   129,    98,   887,   130,   470,
       6,   673,   219,   116,   124,   322,   323,   100,   673,   125,
     101,    96,   703,   117,   104,   106,   562,    97,   126,   299,
     527,   698,   898,   222,   760,    98,   699,   140,   742,   108,
     326,   328,    42,   109,   262,   554,   100,   172,   274,   101,
     277,   111,   333,   104,   106,   645,   788,   287,   888,   361,
     679,   112,   334,   113,    42,    36,   735,   370,   108,    38,
     896,   897,   109,    28,    29,    30,   114,   157,   143,   216,
     111,   219,   685,   115,   310,   556,   147,   377,   148,   378,
     112,   318,   113,   686,   400,   716,   381,   412,   145,   673,
     406,   116,   482,   133,   407,   114,   411,   156,   147,   429,
     128,   117,   115,   251,   435,   350,   126,   355,   274,   433,
     360,   179,   136,   219,   368,   130,     1,     2,     3,   597,
     116,   651,     8,   782,    42,    95,   157,   240,   645,   129,
     117,   205,   205,   439,   442,   126,   443,   156,   124,   491,
     275,   420,   444,   125,   449,    96,   645,   450,   245,   403,
     405,    97,   445,   410,   826,   451,   452,   792,   467,    98,
     754,   456,   416,   417,   673,   457,    42,   458,   459,   759,
     100,   462,    42,   101,   466,   221,   274,   104,   106,   688,
     274,   394,   395,   396,   397,   398,   671,   157,   128,   468,
     472,   793,   108,   157,   671,   777,   109,   645,   355,   854,
     157,   715,   473,    42,   111,   491,   476,   735,   477,   517,
     790,   735,   453,   366,   112,   479,   113,   128,   671,    61,
      36,   673,   520,   530,    38,   671,    28,    29,    30,   114,
     396,   397,   398,     8,   216,   532,   115,   565,   157,   285,
     811,   147,   572,   148,    42,   405,   405,    42,   548,   475,
     549,   274,   568,   274,   116,   366,   569,   481,     8,   577,
     584,   172,    42,   578,   117,   645,   594,   673,   365,   126,
     598,   747,   673,    73,    74,    75,   599,   651,    77,   157,
     647,   835,   354,   662,   673,    42,   666,   673,    42,   667,
     669,     8,   674,   306,   516,   692,   693,   694,   695,   673,
     673,   719,   285,   405,   717,   720,   671,   524,   708,   721,
     723,   722,   725,   778,  -170,    42,    42,   731,   739,   785,
     740,   274,   274,   711,   748,   746,   306,   869,   755,   776,
     749,    36,    28,    29,    30,    38,   173,   712,   219,   757,
     761,   762,   219,   485,   772,   216,   780,   783,     8,   869,
     806,   128,   147,   789,   148,   796,    36,   202,   797,   805,
      38,   670,   210,   807,   405,   219,   274,   869,   427,   274,
     216,   676,   431,   813,   205,   814,   817,   147,   819,   148,
     205,   671,   820,   306,   838,   828,   810,   296,   156,    36,
     172,   831,   830,    38,   836,   224,   225,   226,   227,   228,
     229,   230,   231,   216,   485,   842,   843,   657,   844,   845,
     147,   846,   148,   848,   852,   853,   496,   485,   274,   857,
     855,   860,   865,     8,   861,   579,   274,   862,   870,   874,
       8,   554,   856,   172,   544,   859,   871,   877,   671,   889,
     890,     8,   696,   427,   298,   431,    36,   298,   298,   899,
      38,   697,   589,   704,   298,   701,   702,   219,   142,   298,
     216,   767,   878,   879,   538,   306,   655,   147,   555,   148,
     298,   556,     8,   654,   497,   144,   306,     8,   159,   867,
       8,   298,   342,     8,   671,   274,   775,   351,   298,   671,
     298,   840,   219,   841,   880,   872,   485,   134,   687,   523,
       0,   671,     0,   205,   671,     0,     0,   270,     0,     0,
       0,     0,   270,   535,   537,   792,   671,   671,   357,     0,
       0,    36,     0,     0,   144,    38,     0,   557,    36,   144,
       0,     0,    38,     0,     0,   408,   730,   267,   269,    36,
       0,     0,   216,    38,   148,     0,     0,     0,     8,   147,
       0,   148,     0,   216,     0,     0,     0,     0,   523,     8,
     147,   535,   148,     0,     0,     0,     0,   219,     0,     0,
      36,     0,     0,     0,    38,    36,   219,     0,    36,    38,
       0,    36,    38,   357,   408,    38,     0,     0,     0,   534,
       0,     0,   216,   271,   357,   359,     0,     0,   271,   147,
       0,   148,   219,     0,   271,     0,     0,     0,     0,     0,
     661,     0,     0,   382,   383,   384,   385,   219,   664,   684,
     219,   795,     0,     0,     0,   369,   802,   371,   372,   373,
     374,   375,   376,   390,   391,   392,   393,   394,   395,   396,
     397,   398,     0,     0,     0,     0,    36,   219,     0,     0,
      38,     0,     0,     0,     0,     0,     0,    36,   413,     0,
     430,    38,   219,     0,     0,     0,     0,     0,     0,   271,
       0,   534,   823,   423,   382,   383,   426,   661,     0,     0,
     271,     0,     0,     0,   481,     0,     0,     0,     0,     0,
     728,     0,     0,     0,     0,     0,   392,   393,   394,   395,
     396,   397,   398,   382,   383,   384,   385,     0,   541,     0,
     298,   546,     0,     0,     0,     0,     0,     0,   553,   560,
     563,   388,   389,   390,   391,   392,   393,   394,   395,   396,
     397,   398,   205,   866,   219,     0,     0,   298,     0,   560,
       0,   492,     0,     0,   823,     0,   592,     0,     0,   770,
       0,     0,   172,     0,     0,     0,   219,   224,   225,   226,
     227,   228,   229,   230,   231,   392,   393,   394,   395,   396,
     397,   398,     0,   791,   219,   498,   499,   500,   501,   502,
     503,   504,   505,   506,   507,   508,   509,   510,   511,   512,
     513,   514,     0,     0,     0,     0,   382,   383,   384,   385,
     386,     0,     0,     0,     0,   522,     0,     0,     0,     0,
       0,     0,   529,   387,   388,   389,   390,   493,   392,   393,
     394,   395,   396,   397,   494,     0,     0,     0,     0,     0,
       0,     0,     0,   606,     0,  -464,    57,     0,   560,    58,
      59,    60,     0,     0,     0,   682,     0,   560,     0,     0,
       0,    61,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,
    -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,
       0,     0,     0,   607,    63,     0,     0,  -464,     0,  -464,
    -464,  -464,  -464,  -464,     0,   648,   507,   514,     0,     0,
      65,    66,    67,    68,   608,    70,    71,    72,  -464,  -464,
    -464,   609,   610,   611,     0,    73,   612,    75,     0,    76,
      77,    78,     0,     0,     0,    82,     0,    84,    85,    86,
      87,    88,    89,     0,     0,     0,     0,     0,     0,     0,
       0,    90,     0,  -464,     0,     0,    91,  -464,  -464,   298,
     298,     0,     0,     0,     0,   560,     0,   751,     0,   298,
       8,     0,     0,   172,     0,     0,   613,   223,   224,   225,
     226,   227,   228,   229,   230,   231,     0,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,   232,     0,     0,     0,     0,
       0,     0,     0,    27,    28,    29,    30,    31,    32,     0,
     233,     0,   863,     0,     0,     0,     0,     0,     0,   382,
     383,   384,   385,    33,    34,    35,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   560,
     391,   392,   393,   394,   395,   396,   397,   398,     0,     0,
       0,     0,   737,   374,     0,   738,     0,     0,    36,   741,
       0,    37,    38,     0,     0,   382,   383,   384,   385,   386,
       0,     0,   234,     0,     0,   235,   236,     0,     0,   237,
     238,   239,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     8,     0,     0,   172,     0,
       0,   774,   223,   224,   225,   226,   227,   228,   229,   230,
     231,   414,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
     232,     0,   804,     0,     0,     0,     0,     0,     0,    28,
      29,    30,    31,    32,     0,   233,     0,     0,   266,   382,
     383,   384,   385,   386,     0,     0,     0,     0,    33,    34,
      35,     0,     0,     0,     0,     0,   387,   388,   389,   390,
     391,   392,   393,   394,   395,   396,   397,   398,   382,   383,
     384,   385,     0,     0,     0,     0,   837,     0,     0,     0,
       0,     0,     0,    36,     0,     0,     0,    38,     0,     0,
     392,   393,   394,   395,   396,   397,   398,   234,     0,     0,
     235,   236,     0,     0,   237,   238,   239,     8,     0,     0,
     172,     0,     0,     0,   223,   224,   225,   226,   227,   228,
     229,   230,   231,   531,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,   232,     0,     0,     0,     0,     0,     0,     0,
       0,    28,    29,    30,    31,    32,     0,   233,     0,     0,
     422,   382,   383,   384,   385,   386,     0,     0,     0,     0,
      33,    34,    35,   382,   383,   384,   385,   386,   387,   388,
     389,   390,   391,   392,   393,   394,   395,   396,   397,   398,
     387,   388,   389,   390,   391,   392,   393,   394,   395,   396,
     397,   398,     0,     0,     0,    36,     0,     0,     0,    38,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   234,
       0,     0,   235,   236,     0,     0,   237,   238,   239,     8,
       0,     0,   172,     0,     0,     0,   223,   224,   225,   226,
     227,   228,   229,   230,   231,   533,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,   232,     0,     0,     0,     0,     0,
       0,     0,     0,    28,    29,    30,    31,    32,     0,   233,
       0,     0,   425,   382,   383,   384,   385,   386,     0,     0,
       0,     0,    33,    34,    35,   382,   383,   384,   385,     0,
     387,   388,   389,   390,   391,   392,   393,   394,   395,   396,
     397,   398,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,     0,     0,     0,    36,     0,     0,
       0,    38,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   234,     0,     0,   235,   236,     0,     0,   237,   238,
     239,     8,     0,     0,   172,     0,     0,     0,   223,   224,
     225,   226,   227,   228,   229,   230,   231,     0,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,   232,     0,     0,     0,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
       0,   233,     0,     0,   528,     0,     0,     0,     0,     0,
       8,     0,     0,   172,    33,    34,    35,   223,   224,   225,
     226,   227,   228,   229,   230,   231,     0,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,   232,   649,     0,     0,    36,
       0,     0,     0,    38,    28,    29,    30,    31,    32,     0,
     233,     0,     0,   234,     0,     0,   235,   236,     0,     0,
     237,   238,   239,    33,    34,    35,   382,   383,   384,   385,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   389,   390,   391,   392,   393,
     394,   395,   396,   397,   398,     0,     0,     0,    36,     0,
       0,     0,    38,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   234,     0,     0,   235,   236,     0,     0,   237,
     238,   239,     8,     0,     0,   172,     0,     0,     0,   223,
     224,   225,   226,   227,   228,   229,   230,   231,     0,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,   232,     0,     0,
       0,     0,     0,     0,     0,     0,    28,    29,    30,    31,
      32,     0,   233,     0,     0,     0,     0,     0,     0,     0,
       0,     8,     0,     0,   172,    33,    34,    35,   223,   224,
     225,   226,   227,   228,   229,   230,   231,     0,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,   232,     0,     0,     0,
      36,     0,     0,     0,    38,    28,    29,    30,    31,    32,
       8,   233,     0,     0,   234,     0,     0,   235,   236,     0,
       0,   237,   238,   239,    33,    34,    35,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,   257,     0,     0,     0,     0,
       0,     0,     0,    27,    28,    29,    30,    31,    32,    36,
       0,     0,   144,    38,     0,     0,     0,     0,     0,     0,
       0,     8,     0,    33,    34,    35,   235,   236,     0,     0,
     650,   238,   239,     0,     0,     0,     0,     0,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,   257,     0,    36,     0,
       0,    37,    38,     0,    27,    28,    29,    30,    31,    32,
       0,     0,   258,   144,     0,     0,     0,     0,     0,   147,
       0,     8,     0,     0,    33,    34,    35,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,     0,     0,     0,    36,
       0,     0,    37,    38,    27,    28,    29,    30,    31,    32,
       0,     0,     0,   401,     0,     0,     0,     0,     8,     0,
     147,     0,     0,     0,    33,    34,    35,     0,     0,   659,
       0,     0,     0,     0,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,     0,     0,     0,     0,     0,     0,    36,
       0,     0,    37,    38,     0,    31,    32,   382,   383,   384,
     385,   386,     0,   353,     0,     0,     0,     0,     0,     0,
     147,    33,    34,    35,   387,   388,   389,   390,   391,   392,
     393,   394,   395,   396,   397,   398,     9,    10,    11,    12,
      13,    14,    15,    16,     0,    18,     0,    20,     0,     0,
      23,    24,    25,    26,     0,     0,    36,     0,     0,     0,
      38,    -2,    56,     0,  -464,    57,     0,     0,    58,    59,
      60,     0,     0,     0,     0,     0,     0,   147,     0,     0,
      61,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,
    -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,     0,
       0,     0,    62,    63,     0,     0,     0,     0,  -464,  -464,
    -464,  -464,  -464,     0,     0,    64,     0,     0,     0,    65,
      66,    67,    68,    69,    70,    71,    72,  -464,  -464,  -464,
       0,     0,     0,     0,    73,    74,    75,     0,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,     0,     0,     0,     0,     0,     0,     0,     0,
      90,    56,  -464,  -464,    57,    91,  -464,    58,    59,    60,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
    -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,
    -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,     0,     0,
       0,    62,    63,     0,     0,   570,     0,  -464,  -464,  -464,
    -464,  -464,     0,     0,    64,     0,     0,     0,    65,    66,
      67,    68,    69,    70,    71,    72,  -464,  -464,  -464,     0,
       0,     0,     0,    73,    74,    75,     0,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,     0,     0,     0,     0,     0,     0,     0,     0,    90,
      56,  -464,  -464,    57,    91,  -464,    58,    59,    60,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,  -464,
    -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,
    -464,  -464,  -464,  -464,  -464,  -464,  -464,     0,     0,     0,
      62,    63,     0,     0,   665,     0,  -464,  -464,  -464,  -464,
    -464,     0,     0,    64,     0,     0,     0,    65,    66,    67,
      68,    69,    70,    71,    72,  -464,  -464,  -464,     0,     0,
       0,     0,    73,    74,    75,     0,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
       0,     0,     0,     0,     0,     0,     0,     0,    90,    56,
    -464,  -464,    57,    91,  -464,    58,    59,    60,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,  -464,  -464,
    -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,
    -464,  -464,  -464,  -464,  -464,  -464,     0,     0,     0,    62,
      63,     0,     0,   681,     0,  -464,  -464,  -464,  -464,  -464,
       0,     0,    64,     0,     0,     0,    65,    66,    67,    68,
      69,    70,    71,    72,  -464,  -464,  -464,     0,     0,     0,
       0,    73,    74,    75,     0,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,     0,
       0,     0,     0,     0,     0,     0,     0,    90,    56,  -464,
    -464,    57,    91,  -464,    58,    59,    60,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,  -464,  -464,  -464,
    -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,  -464,
    -464,  -464,  -464,  -464,  -464,     0,     0,     0,    62,    63,
       0,     0,     0,     0,  -464,  -464,  -464,  -464,  -464,     0,
       0,    64,     0,   771,     0,    65,    66,    67,    68,    69,
      70,    71,    72,  -464,  -464,  -464,     0,     0,     0,     0,
      73,    74,    75,     0,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,     7,     0,
       8,     0,     0,     0,     0,     0,    90,     0,  -464,     0,
       0,    91,  -464,     0,     0,     0,     0,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,     0,     0,     0,     0,     0,
       0,     0,     0,    27,    28,    29,    30,    31,    32,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    51,     0,
       8,     0,     0,    33,    34,    35,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,     0,     0,     0,    36,     0,
       0,    37,    38,    27,    28,    29,    30,    31,    32,     0,
       0,     0,     0,     0,     0,     0,     0,   177,     0,   178,
       0,     0,     0,    33,    34,    35,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,     0,     0,     0,     0,    36,     0,
       0,    37,    38,    28,    29,    30,    31,    32,     8,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    33,    34,    35,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    28,    29,    30,    31,    32,    36,     0,     0,
       0,    38,     0,     0,     0,     0,     0,     0,     8,     0,
       0,   220,    34,    35,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,     0,     0,     0,    36,     0,     0,     0,
      38,   727,    28,    29,    30,    31,    32,     8,     0,     0,
       0,     0,     0,     0,     0,   314,     0,     0,     0,     0,
       0,    33,    34,    35,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,     0,     0,     0,     0,     0,     0,     0,     0,
      27,    28,    29,    30,    31,    32,    36,     0,     0,     0,
      38,   727,     0,     0,     0,     0,     0,     8,     0,     0,
      33,    34,    35,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,     0,     0,     0,    36,     0,     0,    37,    38,
      27,    28,    29,    30,    31,    32,     8,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      33,    34,    35,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,   203,     0,     0,     0,     0,     0,     0,     0,     0,
      28,    29,    30,    31,    32,    36,     0,     0,    37,    38,
       0,     0,     0,     0,     0,     8,     0,     0,     0,    33,
      34,    35,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
       0,     0,     0,     0,    36,     0,     0,     0,    38,    28,
      29,    30,    31,    32,     8,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   220,    34,
      35,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    28,    29,
      30,    31,    32,    36,     0,     0,     0,    38,     0,     0,
       0,     0,     0,   683,     0,     0,     0,    33,    34,    35,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,     0,     0,
       0,     0,    36,     0,     0,     0,    38,    28,    29,    30,
      31,    32,     8,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    33,    34,    35,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,   656,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    36,   663,     0,     0,    38,     0,     0,     0,   382,
     383,   384,   385,   386,     0,    33,    34,    35,     0,     0,
       0,     0,     0,     0,     0,     0,   387,   388,   389,   390,
     391,   392,   393,   394,   395,   396,   397,   398,     0,     0,
     382,   383,   384,   385,   386,     0,     0,     0,     0,     0,
      36,     0,     0,     0,    38,     0,     0,   387,   388,   389,
     390,   391,   392,   393,   394,   395,   396,   397,   398
};

static const yytype_int16 yycheck[] =
{
       0,    91,   131,    42,   466,     5,    40,   550,     8,   212,
     447,   197,   564,    85,   321,     5,   328,   716,     3,     5,
      39,    21,   305,   438,   142,     1,     2,   729,     5,   215,
      41,   146,   447,     5,     1,     5,   864,    39,     3,   129,
     155,     5,    42,     6,    39,   559,     1,     2,   138,     5,
     137,     0,    69,   567,   882,    38,    40,    38,    40,     3,
       5,   784,   258,     5,    64,   261,    91,     5,     5,    69,
      70,    38,    55,    41,   161,   162,    44,   591,   777,     3,
      91,     3,     5,   126,   598,   102,     5,   205,     3,    52,
       3,   790,   635,   795,     5,   818,   115,     3,    52,    54,
     102,   171,   102,   565,     5,   175,     5,   102,    92,   832,
      92,   181,   151,    40,   153,    80,   186,     6,    45,     5,
     105,     3,    42,    91,     4,    38,     5,     3,     3,   436,
     254,   131,    38,     3,   115,   106,   101,    91,   208,   257,
     105,   122,    55,   258,     5,   457,   261,   217,   455,    55,
       3,   151,   270,   153,     5,   131,    38,   353,   354,     5,
     883,   105,    38,    46,    44,   679,    55,   291,   168,   284,
     869,    38,     3,    55,    44,   718,   131,   101,   178,    55,
       4,   105,     6,   105,     4,   110,   101,   142,   101,    91,
     105,   191,   105,   736,     4,   101,     6,    72,   122,   105,
     752,     3,   115,     3,    38,   205,   245,   207,   208,   122,
     672,   124,   212,    44,     3,   411,    52,     6,   124,   101,
     220,    55,   142,   105,    44,   101,   101,   147,     3,   105,
     105,   101,   187,   188,    44,   105,   212,   674,    41,   115,
     754,     5,   124,   243,   787,   245,    54,     6,   203,   378,
      39,   437,   105,     3,   179,    91,   211,   212,    41,   674,
      43,    41,   334,    38,    44,   468,     6,    38,   307,     3,
     101,     3,     6,     6,   105,     4,   472,     6,   464,     4,
     205,     6,    41,   590,    55,   205,   401,   402,    38,     3,
     103,    41,    72,   408,   756,   215,   221,   811,     3,   101,
     415,   101,   257,   105,    44,   105,    38,   307,    40,     3,
     400,    44,   855,   126,   429,   270,    41,   293,    41,    44,
      52,   321,   103,    55,   279,   245,   101,   282,    40,    41,
     105,    41,    44,    38,    47,    48,    49,   257,   293,     3,
      72,     3,     6,   857,    38,   126,   346,     6,   862,   124,
     270,   101,   352,    38,    39,   105,   276,   140,    52,    19,
      92,    55,   362,    38,   147,   115,   366,   101,   368,   101,
      55,   105,   122,   105,   124,   100,    38,   447,    72,   187,
     188,   115,    42,   115,    52,    38,   306,   101,   122,    38,
     122,   105,   124,    39,    40,   203,   101,    47,    48,    49,
     105,   115,   378,   211,    72,   520,     3,   101,   122,   122,
     365,   105,    40,    73,    74,    75,    38,    39,    78,   124,
     763,   115,   765,   378,    52,    38,    76,    38,   122,   712,
     124,    38,    39,    55,   434,     3,   436,   357,    38,   101,
      91,    38,    55,   105,   104,   365,   485,    41,    55,   369,
      44,   102,   103,   115,   454,   455,   456,   496,    55,    38,
     122,   461,   124,   418,   454,    38,    38,    52,   454,    41,
     456,   279,    44,   449,   282,   126,    38,   454,    38,   479,
     666,   667,   454,    38,   454,   485,   486,    72,    38,   465,
     454,    76,   468,    46,   449,    44,   496,   497,   454,    39,
      40,    69,    70,    71,   101,    50,    51,   102,   105,   454,
     465,    41,   454,   468,    44,    33,   454,   454,   115,    38,
      39,   446,    67,    68,    42,   450,   613,   124,    38,    39,
      48,   454,     3,   101,    39,   454,    55,   105,    41,    39,
      40,    44,   542,   454,    40,    55,    91,     3,   473,    52,
     550,    41,   542,   454,   644,   454,   542,   365,     3,   559,
      78,    79,    50,    51,   693,   542,    41,   567,   454,    44,
     542,   571,   542,     3,   494,   454,   496,    41,   542,    67,
      68,   571,    38,   366,    41,   571,   542,    44,   588,   589,
     590,   591,   110,   454,   571,    44,     3,   542,   598,   571,
     542,   571,   588,   454,   542,   542,   676,   571,   454,     6,
     418,   588,   895,   703,   686,   571,   588,   651,   657,   542,
       3,    44,   577,   542,   142,     4,   571,     6,   146,   571,
     148,   542,    41,   571,   571,   635,   723,   155,   877,   102,
     565,   542,    40,   542,   599,   101,   646,    38,   571,   105,
     889,   890,   571,    47,    48,    49,   542,   657,    52,   115,
     571,   179,    41,   542,   182,    44,   122,    92,   124,    40,
     571,   189,   571,    52,    92,   600,    38,    55,    72,   679,
      39,   542,    76,   683,    39,   571,   102,   205,   122,   102,
     454,   542,   571,   693,    39,   213,   542,   215,   216,    41,
     218,    65,   220,   221,   222,   705,   107,   108,   109,   709,
     571,   494,     3,   713,   669,   705,   716,   693,   718,   705,
     571,    85,    86,    39,    39,   571,    44,   245,   705,   729,
     650,   651,    39,   705,    40,   705,   736,    39,   693,   257,
     258,   705,    52,   261,   783,    39,    41,    38,    92,   705,
     675,    39,   270,   271,   754,    39,   711,    39,    39,   684,
     705,    39,   717,   705,    39,   129,   284,   705,   705,   577,
     288,   118,   119,   120,   121,   122,   559,   777,   542,    40,
     102,    72,   705,   783,   567,   710,   705,   787,   306,   828,
     790,   599,    39,   748,   705,   795,    41,   797,   102,   102,
     725,   801,   320,   728,   705,    44,   705,   571,   591,    19,
     101,   811,   102,    39,   105,   598,    47,    48,    49,   705,
     120,   121,   122,     3,   115,    39,   705,    39,   828,   155,
     755,   122,    42,   124,   789,   353,   354,   792,    41,   357,
      41,   359,    44,   361,   705,   770,    41,   365,     3,    38,
      41,     6,   807,    45,   705,   855,     3,   857,    38,   705,
      39,   669,   862,    73,    74,    75,    38,   650,    78,   869,
      44,   791,   792,    39,   874,   830,    40,   877,   833,    40,
      38,     3,    52,    38,   402,   126,    91,    39,    45,   889,
     890,    44,   218,   411,    38,    72,   679,   415,    52,    72,
     124,    72,    45,   711,    45,   860,   861,    74,    39,   717,
      39,   429,   430,    38,    38,    45,    38,   842,    39,    41,
      76,   101,    47,    48,    49,   105,    63,    52,   446,    41,
      39,     3,   450,   716,    41,   115,    72,    39,     3,   864,
     748,   705,   122,    38,   124,    41,   101,    84,    40,    39,
     105,    76,    89,    38,   472,   473,   474,   882,   284,   477,
     115,    40,   288,    41,   328,    41,    92,   122,    39,   124,
     334,   754,    41,    38,    44,    39,    41,     3,   496,   101,
       6,   789,    38,   105,   792,    11,    12,    13,    14,    15,
      16,    17,    18,   115,   777,    45,    41,   515,    39,   807,
     122,    41,   124,    41,    40,    38,   370,   790,   526,    39,
      45,    38,    44,     3,    38,   461,   534,    39,    39,    39,
       3,     4,   830,     6,   438,   833,    41,    39,   811,    39,
      39,     3,   588,   359,   171,   361,   101,   174,   175,    41,
     105,   588,   468,   589,   181,   588,   588,   565,    38,   186,
     115,   693,   860,   861,   434,    38,   497,   122,    41,   124,
     197,    44,     3,   496,   378,    55,    38,     3,    46,    41,
       3,   208,   209,     3,   857,   593,   709,   214,   215,   862,
     217,   797,   600,   801,   862,   852,   869,    21,   574,   415,
      -1,   874,    -1,   457,   877,    -1,    -1,    38,    -1,    -1,
      -1,    -1,    38,   429,   430,    38,   889,   890,    38,    -1,
      -1,   101,    -1,    -1,    55,   105,    -1,   100,   101,    55,
      -1,    -1,   105,    -1,    -1,   115,   644,   144,   145,   101,
      -1,    -1,   115,   105,   124,    -1,    -1,    -1,     3,   122,
      -1,   124,    -1,   115,    -1,    -1,    -1,    -1,   474,     3,
     122,   477,   124,    -1,    -1,    -1,    -1,   675,    -1,    -1,
     101,    -1,    -1,    -1,   105,   101,   684,    -1,   101,   105,
      -1,   101,   105,    38,   115,   105,    -1,    -1,    -1,   115,
      -1,    -1,   115,   124,    38,   115,    -1,    -1,   124,   122,
      -1,   124,   710,    -1,   124,    -1,    -1,    -1,    -1,    -1,
     526,    -1,    -1,    94,    95,    96,    97,   725,   534,   573,
     728,   729,    -1,    -1,    -1,   232,   734,   234,   235,   236,
     237,   238,   239,   114,   115,   116,   117,   118,   119,   120,
     121,   122,    -1,    -1,    -1,    -1,   101,   755,    -1,    -1,
     105,    -1,    -1,    -1,    -1,    -1,    -1,   101,   265,    -1,
     115,   105,   770,    -1,    -1,    -1,    -1,    -1,    -1,   124,
      -1,   115,   780,   280,    94,    95,   283,   593,    -1,    -1,
     124,    -1,    -1,    -1,   792,    -1,    -1,    -1,    -1,    -1,
     644,    -1,    -1,    -1,    -1,    -1,   116,   117,   118,   119,
     120,   121,   122,    94,    95,    96,    97,    -1,   435,    -1,
     437,   438,    -1,    -1,    -1,    -1,    -1,    -1,   445,   446,
     447,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   686,   841,   842,    -1,    -1,   464,    -1,   466,
      -1,    39,    -1,    -1,   852,    -1,   473,    -1,    -1,   703,
      -1,    -1,     6,    -1,    -1,    -1,   864,    11,    12,    13,
      14,    15,    16,    17,    18,   116,   117,   118,   119,   120,
     121,   122,    -1,   727,   882,   382,   383,   384,   385,   386,
     387,   388,   389,   390,   391,   392,   393,   394,   395,   396,
     397,   398,    -1,    -1,    -1,    -1,    94,    95,    96,    97,
      98,    -1,    -1,    -1,    -1,   412,    -1,    -1,    -1,    -1,
      -1,    -1,   419,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,     3,     4,    -1,   565,     7,
       8,     9,    -1,    -1,    -1,   572,    -1,   574,    -1,    -1,
      -1,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      -1,    -1,    -1,    41,    42,    -1,    -1,    45,    -1,    47,
      48,    49,    50,    51,    -1,   492,   493,   494,    -1,    -1,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    -1,    73,    74,    75,    -1,    77,
      78,    79,    -1,    -1,    -1,    83,    -1,    85,    86,    87,
      88,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,   101,    -1,    -1,   104,   105,   106,   666,
     667,    -1,    -1,    -1,    -1,   672,    -1,   674,    -1,   676,
       3,    -1,    -1,     6,    -1,    -1,   124,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    47,    48,    49,    50,    51,    -1,
      53,    -1,    41,    -1,    -1,    -1,    -1,    -1,    -1,    94,
      95,    96,    97,    66,    67,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   756,
     115,   116,   117,   118,   119,   120,   121,   122,    -1,    -1,
      -1,    -1,   649,   650,    -1,   652,    -1,    -1,   101,   656,
      -1,   104,   105,    -1,    -1,    94,    95,    96,    97,    98,
      -1,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,   122,
     123,   124,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,     6,    -1,
      -1,   708,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    56,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    -1,   739,    -1,    -1,    -1,    -1,    -1,    -1,    47,
      48,    49,    50,    51,    -1,    53,    -1,    -1,    56,    94,
      95,    96,    97,    98,    -1,    -1,    -1,    -1,    66,    67,
      68,    -1,    -1,    -1,    -1,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,    94,    95,
      96,    97,    -1,    -1,    -1,    -1,   793,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,
     116,   117,   118,   119,   120,   121,   122,   115,    -1,    -1,
     118,   119,    -1,    -1,   122,   123,   124,     3,    -1,    -1,
       6,    -1,    -1,    -1,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    56,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    47,    48,    49,    50,    51,    -1,    53,    -1,    -1,
      56,    94,    95,    96,    97,    98,    -1,    -1,    -1,    -1,
      66,    67,    68,    94,    95,    96,    97,    98,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
      -1,    -1,   118,   119,    -1,    -1,   122,   123,   124,     3,
      -1,    -1,     6,    -1,    -1,    -1,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    56,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    47,    48,    49,    50,    51,    -1,    53,
      -1,    -1,    56,    94,    95,    96,    97,    98,    -1,    -1,
      -1,    -1,    66,    67,    68,    94,    95,    96,    97,    -1,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,    -1,   122,   123,
     124,     3,    -1,    -1,     6,    -1,    -1,    -1,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    47,    48,    49,    50,    51,
      -1,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,
       3,    -1,    -1,     6,    66,    67,    68,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,   101,
      -1,    -1,    -1,   105,    47,    48,    49,    50,    51,    -1,
      53,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,   124,    66,    67,    68,    94,    95,    96,    97,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,    -1,    -1,    -1,   101,    -1,
      -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,   122,
     123,   124,     3,    -1,    -1,     6,    -1,    -1,    -1,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    47,    48,    49,    50,
      51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,    -1,    -1,     6,    66,    67,    68,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    -1,    -1,    -1,
     101,    -1,    -1,    -1,   105,    47,    48,    49,    50,    51,
       3,    53,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,
      -1,   122,   123,   124,    66,    67,    68,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    47,    48,    49,    50,    51,   101,
      -1,    -1,    55,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,    -1,    66,    67,    68,   118,   119,    -1,    -1,
     122,   123,   124,    -1,    -1,    -1,    -1,    -1,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    -1,   101,    -1,
      -1,   104,   105,    -1,    46,    47,    48,    49,    50,    51,
      -1,    -1,   115,    55,    -1,    -1,    -1,    -1,    -1,   122,
      -1,     3,    -1,    -1,    66,    67,    68,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,    46,    47,    48,    49,    50,    51,
      -1,    -1,    -1,   115,    -1,    -1,    -1,    -1,     3,    -1,
     122,    -1,    -1,    -1,    66,    67,    68,    -1,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,    -1,    50,    51,    94,    95,    96,
      97,    98,    -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,
     122,    66,    67,    68,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,    20,    21,    22,    23,
      24,    25,    26,    27,    -1,    29,    -1,    31,    -1,    -1,
      34,    35,    36,    37,    -1,    -1,   101,    -1,    -1,    -1,
     105,     0,     1,    -1,     3,     4,    -1,    -1,     7,     8,
       9,    -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,    -1,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    -1,
      -1,    -1,    41,    42,    -1,    -1,    -1,    -1,    47,    48,
      49,    50,    51,    -1,    -1,    54,    -1,    -1,    -1,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      -1,    -1,    -1,    -1,    73,    74,    75,    -1,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,     1,   101,     3,     4,   104,   105,     7,     8,     9,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    -1,    -1,
      -1,    41,    42,    -1,    -1,    45,    -1,    47,    48,    49,
      50,    51,    -1,    -1,    54,    -1,    -1,    -1,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    -1,
      -1,    -1,    -1,    73,    74,    75,    -1,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
       1,   101,     3,     4,   104,   105,     7,     8,     9,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    -1,    -1,    -1,
      41,    42,    -1,    -1,    45,    -1,    47,    48,    49,    50,
      51,    -1,    -1,    54,    -1,    -1,    -1,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    -1,    -1,
      -1,    -1,    73,    74,    75,    -1,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,     1,
     101,     3,     4,   104,   105,     7,     8,     9,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    -1,    -1,    -1,    41,
      42,    -1,    -1,    45,    -1,    47,    48,    49,    50,    51,
      -1,    -1,    54,    -1,    -1,    -1,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    -1,    -1,    -1,
      -1,    73,    74,    75,    -1,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,     1,   101,
       3,     4,   104,   105,     7,     8,     9,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    -1,    -1,    -1,    41,    42,
      -1,    -1,    -1,    -1,    47,    48,    49,    50,    51,    -1,
      -1,    54,    -1,    56,    -1,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    -1,    -1,    -1,    -1,
      73,    74,    75,    -1,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,     1,    -1,
       3,    -1,    -1,    -1,    -1,    -1,    99,    -1,   101,    -1,
      -1,   104,   105,    -1,    -1,    -1,    -1,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    47,    48,    49,    50,    51,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
       3,    -1,    -1,    66,    67,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    -1,    -1,    -1,   101,    -1,
      -1,   104,   105,    46,    47,    48,    49,    50,    51,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,     3,
      -1,    -1,    -1,    66,    67,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    -1,    -1,    -1,    -1,   101,    -1,
      -1,   104,   105,    47,    48,    49,    50,    51,     3,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    66,    67,    68,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    47,    48,    49,    50,    51,   101,    -1,    -1,
      -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,
      -1,    66,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    -1,    -1,    -1,   101,    -1,    -1,    -1,
     105,   106,    47,    48,    49,    50,    51,     3,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,
      -1,    66,    67,    68,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    47,    48,    49,    50,    51,   101,    -1,    -1,    -1,
     105,   106,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,
      66,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,
      46,    47,    48,    49,    50,    51,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      66,    67,    68,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      47,    48,    49,    50,    51,   101,    -1,    -1,   104,   105,
      -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,    66,
      67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    47,
      48,    49,    50,    51,     3,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    66,    67,
      68,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    47,    48,
      49,    50,    51,   101,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,     3,    -1,    -1,    -1,    66,    67,    68,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,    47,    48,    49,
      50,    51,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    66,    67,    68,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      51,   101,    56,    -1,    -1,   105,    -1,    -1,    -1,    94,
      95,    96,    97,    98,    -1,    66,    67,    68,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,    -1,    -1,
      94,    95,    96,    97,    98,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,   105,    -1,    -1,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   107,   108,   109,   128,   129,   272,     1,     3,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    46,    47,    48,
      49,    50,    51,    66,    67,    68,   101,   104,   105,   213,
     227,   228,   230,   231,   232,   233,   234,   251,   252,   262,
     264,     1,   213,     1,    38,     0,     1,     4,     7,     8,
       9,    19,    41,    42,    54,    58,    59,    60,    61,    62,
      63,    64,    65,    73,    74,    75,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      99,   104,   130,   131,   132,   134,   135,   136,   137,   138,
     141,   142,   144,   145,   146,   147,   148,   149,   150,   153,
     154,   155,   158,   160,   165,   166,   167,   168,   170,   173,
     174,   175,   176,   177,   181,   182,   189,   190,   200,   209,
     272,    91,   259,   272,   259,    46,   262,   126,    91,    41,
     231,   227,    38,    52,    55,    72,   115,   122,   124,   218,
     219,   221,   223,   224,   225,   226,   262,   272,   227,   233,
     262,   103,   126,   263,    41,    41,   210,   211,   213,   272,
     106,    38,     6,   267,    38,   269,   272,     1,     3,   229,
     230,    38,   269,    38,   152,   272,    38,    38,    38,    80,
     262,     3,    44,   262,    38,     4,    44,    38,    38,    41,
      44,     4,   267,    38,   164,   229,   162,   164,    38,    38,
     267,    38,    91,   252,   269,    38,   115,   221,   226,   262,
      66,   229,   252,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    38,    53,   115,   118,   119,   122,   123,   124,
     213,   214,   215,   217,   229,   230,   241,   242,   243,   244,
     267,   272,    46,   105,   264,   252,   227,    38,   115,   210,
     224,   226,   262,    44,   235,   236,    56,   241,   242,   241,
      38,   124,   222,   225,   262,   226,   227,   262,   218,    38,
      55,   218,    38,    55,   115,   222,   225,   262,   102,   264,
     105,   264,    39,    40,   212,   272,     3,   260,   267,     6,
      44,   260,   270,   260,    41,    52,    38,   221,    39,   260,
     262,     3,     3,   260,    11,   159,   210,   210,   262,    41,
      52,   192,    44,     3,   161,   270,     3,   210,    44,   220,
     221,   224,   272,    41,    40,   163,   272,   260,   261,   272,
     139,   140,   267,   210,   185,   186,   187,   213,   251,   272,
     262,   267,     3,   115,   226,   262,   270,    38,   260,   115,
     262,   102,     3,   237,   272,    38,   221,    44,   262,   241,
      38,   241,   241,   241,   241,   241,   241,    92,    40,   216,
     272,    38,    94,    95,    96,    97,    98,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   263,
      92,   115,   226,   262,   223,   262,    39,    39,   115,   223,
     262,   102,    55,   241,    56,   226,   262,   262,    38,    55,
     226,   210,    56,   241,   210,    56,   241,   222,   225,   102,
     115,   222,   263,    41,   213,    39,   169,    40,    52,    39,
     235,   218,    39,    44,    39,    52,    39,    40,   157,    40,
      39,    39,    41,   262,   129,   191,    39,    39,    39,    39,
     162,   164,    39,    39,    40,    44,    39,    92,    40,   188,
     272,    55,   102,    39,   226,   262,    41,   102,    41,    44,
     210,   262,    76,   172,   218,   227,   179,    41,    72,   245,
     246,   272,    39,   115,   122,   226,   229,   217,   241,   241,
     241,   241,   241,   241,   241,   241,   241,   241,   241,   241,
     241,   241,   241,   241,   241,   252,   262,   102,    39,    39,
     102,   223,   241,   222,   262,    39,   102,   210,    56,   241,
      39,    56,    39,    56,   115,   222,   225,   222,   212,     4,
      44,   267,   129,   270,   139,   243,   267,   271,    41,    41,
     133,     4,   151,   267,     4,    41,    44,   100,   156,   221,
     267,   268,   260,   267,   271,    39,   213,   221,    44,    41,
      45,   129,    42,   209,   162,    41,    44,    38,    45,   163,
       3,   101,   105,   265,    41,   270,   213,   156,   183,   187,
     143,   221,   267,   102,     3,   238,   239,   272,    39,    38,
      40,    41,    44,   171,    76,   218,     1,    41,    62,    69,
      70,    71,    74,   124,   134,   135,   136,   137,   141,   142,
     146,   148,   150,   153,   155,   158,   160,   165,   166,   167,
     168,   181,   182,   189,   193,   196,   197,   198,   199,   200,
     201,   202,   205,   208,   209,   272,   247,    44,   241,    39,
     122,   227,    39,   115,   219,   216,    72,   262,    39,    56,
      39,   222,    39,    56,   222,    45,    40,    40,   193,    38,
      76,   227,   254,   272,    52,    39,    40,   157,   156,   221,
     254,    45,   267,     3,   229,    41,    52,   268,   210,   103,
     126,   266,   126,    91,    39,    45,   170,   177,   181,   182,
     184,   197,   199,   209,   188,   129,   254,    41,    52,    40,
      45,    38,    52,   254,   255,   210,   221,    38,   195,    44,
      72,    72,    72,   124,   264,    45,   193,   106,   229,   252,
     262,    74,   248,   249,   253,   272,   178,   241,   241,    39,
      39,   241,   218,    39,   270,   270,    45,   210,    38,    76,
     156,   267,   271,    41,   221,    39,   254,    41,    41,   221,
     164,    39,     3,     3,   105,     3,   105,   214,     4,    44,
     229,    56,    41,   240,   241,   239,    41,   221,   210,   235,
      72,   256,   272,    39,   172,   210,   193,   194,   264,    38,
     221,   229,    38,    72,   245,   262,    41,    40,    69,    70,
      71,   250,   262,   193,   241,    39,   210,    38,   157,   254,
      41,   221,   156,    41,    41,   266,   266,    92,   172,    39,
      41,   257,   258,   262,    41,    44,   218,   171,    39,   193,
      38,   210,   172,    38,   115,   226,   210,   241,    44,   245,
     249,   253,    45,    41,    39,   210,    41,   254,    41,   171,
      41,    44,    40,    38,   218,    45,   210,    39,   171,   210,
      38,    38,    39,    41,   204,    44,   262,    41,   180,   221,
      39,    41,   258,   193,    39,   206,   254,    39,   210,   210,
     255,   180,   203,   172,   207,   254,    41,    44,   207,    39,
      39,   180,   171,    41,    44,    52,   207,   207,   235,    41
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 1745 "parser.y"
    {
                   if (!classes) classes = NewHash();
		   Setattr((yyvsp[(1) - (1)].node),"classes",classes); 
		   Setattr((yyvsp[(1) - (1)].node),"name",ModuleName);
		   
		   if ((!module_node) && ModuleName) {
		     module_node = new_node("module");
		     Setattr(module_node,"name",ModuleName);
		   }
		   Setattr((yyvsp[(1) - (1)].node),"module",module_node);
		   check_extensions();
	           top = (yyvsp[(1) - (1)].node);
               }
    break;

  case 3:
#line 1758 "parser.y"
    {
                 top = Copy(Getattr((yyvsp[(2) - (3)].p),"type"));
		 Delete((yyvsp[(2) - (3)].p));
               }
    break;

  case 4:
#line 1762 "parser.y"
    {
                 top = 0;
               }
    break;

  case 5:
#line 1765 "parser.y"
    {
                 top = (yyvsp[(2) - (3)].p);
               }
    break;

  case 6:
#line 1768 "parser.y"
    {
                 top = 0;
               }
    break;

  case 7:
#line 1771 "parser.y"
    {
                 top = (yyvsp[(3) - (5)].pl);
               }
    break;

  case 8:
#line 1774 "parser.y"
    {
                 top = 0;
               }
    break;

  case 9:
#line 1779 "parser.y"
    {  
                   /* add declaration to end of linked list (the declaration isn't always a single declaration, sometimes it is a linked list itself) */
                   appendChild((yyvsp[(1) - (2)].node),(yyvsp[(2) - (2)].node));
                   (yyval.node) = (yyvsp[(1) - (2)].node);
               }
    break;

  case 10:
#line 1784 "parser.y"
    {
                   (yyval.node) = new_node("top");
               }
    break;

  case 11:
#line 1789 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 12:
#line 1790 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 13:
#line 1791 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 14:
#line 1792 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 15:
#line 1793 "parser.y"
    {
                  (yyval.node) = 0;
		  Swig_error(cparse_file, cparse_line,"Syntax error in input(1).\n");
		  exit(1);
               }
    break;

  case 16:
#line 1799 "parser.y"
    { 
                  if ((yyval.node)) {
   		      add_symbols((yyval.node));
                  }
                  (yyval.node) = (yyvsp[(1) - (1)].node); 
	       }
    break;

  case 17:
#line 1815 "parser.y"
    {
                  (yyval.node) = 0;
                  skip_decl();
               }
    break;

  case 18:
#line 1825 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 19:
#line 1826 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 20:
#line 1827 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 21:
#line 1828 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 22:
#line 1829 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 23:
#line 1830 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 24:
#line 1831 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 25:
#line 1832 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 26:
#line 1833 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 27:
#line 1834 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 28:
#line 1835 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 29:
#line 1836 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 30:
#line 1837 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 31:
#line 1838 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 32:
#line 1839 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 33:
#line 1840 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 34:
#line 1841 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 35:
#line 1842 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 36:
#line 1843 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 37:
#line 1844 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 38:
#line 1845 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 39:
#line 1852 "parser.y"
    {
               Node *cls;
	       String *clsname;
	       cplus_mode = CPLUS_PUBLIC;
	       if (!classes) classes = NewHash();
	       if (!extendhash) extendhash = NewHash();
	       clsname = make_class_name((yyvsp[(3) - (4)].str));
	       cls = Getattr(classes,clsname);
	       if (!cls) {
		 /* No previous definition. Create a new scope */
		 Node *am = Getattr(extendhash,clsname);
		 if (!am) {
		   Swig_symbol_newscope();
		   Swig_symbol_setscopename((yyvsp[(3) - (4)].str));
		   prev_symtab = 0;
		 } else {
		   prev_symtab = Swig_symbol_setscope(Getattr(am,"symtab"));
		 }
		 current_class = 0;
	       } else {
		 /* Previous class definition.  Use its symbol table */
		 prev_symtab = Swig_symbol_setscope(Getattr(cls,"symtab"));
		 current_class = cls;
		 extendmode = 1;
	       }
	       Classprefix = NewString((yyvsp[(3) - (4)].str));
	       Namespaceprefix= Swig_symbol_qualifiedscopename(0);
	       Delete(clsname);
	     }
    break;

  case 40:
#line 1880 "parser.y"
    {
               String *clsname;
	       extendmode = 0;
               (yyval.node) = new_node("extend");
	       Setattr((yyval.node),"symtab",Swig_symbol_popscope());
	       if (prev_symtab) {
		 Swig_symbol_setscope(prev_symtab);
	       }
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
               clsname = make_class_name((yyvsp[(3) - (7)].str));
	       Setattr((yyval.node),"name",clsname);

	       /* Mark members as extend */

	       tag_nodes((yyvsp[(6) - (7)].node),"feature:extend",(char*) "1");
	       if (current_class) {
		 /* We add the extension to the previously defined class */
		 appendChild((yyval.node),(yyvsp[(6) - (7)].node));
		 appendChild(current_class,(yyval.node));
	       } else {
		 /* We store the extensions in the extensions hash */
		 Node *am = Getattr(extendhash,clsname);
		 if (am) {
		   /* Append the members to the previous extend methods */
		   appendChild(am,(yyvsp[(6) - (7)].node));
		 } else {
		   appendChild((yyval.node),(yyvsp[(6) - (7)].node));
		   Setattr(extendhash,clsname,(yyval.node));
		 }
	       }
	       current_class = 0;
	       Delete(Classprefix);
	       Delete(clsname);
	       Classprefix = 0;
	       prev_symtab = 0;
	       (yyval.node) = 0;

	     }
    break;

  case 41:
#line 1924 "parser.y"
    {
                    (yyval.node) = new_node("apply");
                    Setattr((yyval.node),"pattern",Getattr((yyvsp[(2) - (5)].p),"pattern"));
		    appendChild((yyval.node),(yyvsp[(4) - (5)].p));
               }
    break;

  case 42:
#line 1934 "parser.y"
    {
		 (yyval.node) = new_node("clear");
		 appendChild((yyval.node),(yyvsp[(2) - (3)].p));
               }
    break;

  case 43:
#line 1945 "parser.y"
    {
		   if (((yyvsp[(4) - (5)].dtype).type != T_ERROR) && ((yyvsp[(4) - (5)].dtype).type != T_SYMBOL)) {
		     SwigType *type = NewSwigType((yyvsp[(4) - (5)].dtype).type);
		     (yyval.node) = new_node("constant");
		     Setattr((yyval.node),"name",(yyvsp[(2) - (5)].id));
		     Setattr((yyval.node),"type",type);
		     Setattr((yyval.node),"value",(yyvsp[(4) - (5)].dtype).val);
		     if ((yyvsp[(4) - (5)].dtype).rawval) Setattr((yyval.node),"rawval", (yyvsp[(4) - (5)].dtype).rawval);
		     Setattr((yyval.node),"storage","%constant");
		     SetFlag((yyval.node),"feature:immutable");
		     add_symbols((yyval.node));
		     Delete(type);
		   } else {
		     if ((yyvsp[(4) - (5)].dtype).type == T_ERROR) {
		       Swig_warning(WARN_PARSE_UNSUPPORTED_VALUE,cparse_file,cparse_line,"Unsupported constant value (ignored)\n");
		     }
		     (yyval.node) = 0;
		   }

	       }
    break;

  case 44:
#line 1966 "parser.y"
    {
		 if (((yyvsp[(4) - (5)].dtype).type != T_ERROR) && ((yyvsp[(4) - (5)].dtype).type != T_SYMBOL)) {
		   SwigType_push((yyvsp[(2) - (5)].type),(yyvsp[(3) - (5)].decl).type);
		   /* Sneaky callback function trick */
		   if (SwigType_isfunction((yyvsp[(2) - (5)].type))) {
		     SwigType_add_pointer((yyvsp[(2) - (5)].type));
		   }
		   (yyval.node) = new_node("constant");
		   Setattr((yyval.node),"name",(yyvsp[(3) - (5)].decl).id);
		   Setattr((yyval.node),"type",(yyvsp[(2) - (5)].type));
		   Setattr((yyval.node),"value",(yyvsp[(4) - (5)].dtype).val);
		   if ((yyvsp[(4) - (5)].dtype).rawval) Setattr((yyval.node),"rawval", (yyvsp[(4) - (5)].dtype).rawval);
		   Setattr((yyval.node),"storage","%constant");
		   SetFlag((yyval.node),"feature:immutable");
		   add_symbols((yyval.node));
		 } else {
		     if ((yyvsp[(4) - (5)].dtype).type == T_ERROR) {
		       Swig_warning(WARN_PARSE_UNSUPPORTED_VALUE,cparse_file,cparse_line,"Unsupported constant value\n");
		     }
		   (yyval.node) = 0;
		 }
               }
    break;

  case 45:
#line 1988 "parser.y"
    {
		 Swig_warning(WARN_PARSE_BAD_VALUE,cparse_file,cparse_line,"Bad constant value (ignored).\n");
		 (yyval.node) = 0;
	       }
    break;

  case 46:
#line 1999 "parser.y"
    {
		 char temp[64];
		 Replace((yyvsp[(2) - (2)].str),"$file",cparse_file, DOH_REPLACE_ANY);
		 sprintf(temp,"%d", cparse_line);
		 Replace((yyvsp[(2) - (2)].str),"$line",temp,DOH_REPLACE_ANY);
		 Printf(stderr,"%s\n", (yyvsp[(2) - (2)].str));
		 Delete((yyvsp[(2) - (2)].str));
                 (yyval.node) = 0;
	       }
    break;

  case 47:
#line 2008 "parser.y"
    {
		 char temp[64];
		 String *s = NewString((yyvsp[(2) - (2)].id));
		 Replace(s,"$file",cparse_file, DOH_REPLACE_ANY);
		 sprintf(temp,"%d", cparse_line);
		 Replace(s,"$line",temp,DOH_REPLACE_ANY);
		 Printf(stderr,"%s\n", s);
		 Delete(s);
                 (yyval.node) = 0;
               }
    break;

  case 48:
#line 2027 "parser.y"
    {
                    skip_balanced('{','}');
		    (yyval.node) = 0;
		    Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
	       }
    break;

  case 49:
#line 2033 "parser.y"
    {
                    skip_balanced('{','}');
		    (yyval.node) = 0;
		    Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
               }
    break;

  case 50:
#line 2039 "parser.y"
    {
		 (yyval.node) = 0;
		 Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
               }
    break;

  case 51:
#line 2044 "parser.y"
    {
		 (yyval.node) = 0;
		 Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
	       }
    break;

  case 52:
#line 2051 "parser.y"
    {		 
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"value",(yyvsp[(1) - (4)].id));
		 Setattr((yyval.node),"type",Getattr((yyvsp[(3) - (4)].p),"type"));
               }
    break;

  case 53:
#line 2058 "parser.y"
    {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"value",(yyvsp[(1) - (1)].id));
              }
    break;

  case 54:
#line 2062 "parser.y"
    {
                (yyval.node) = (yyvsp[(1) - (1)].node);
              }
    break;

  case 55:
#line 2075 "parser.y"
    {
                   Hash *p = (yyvsp[(5) - (7)].node);
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[(3) - (7)].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[(3) - (7)].node),"type"));
		   Setattr((yyval.node),"section",Getattr(p,"name"));
		   Setattr((yyval.node),"kwargs",nextSibling(p));
		   Setattr((yyval.node),"code",(yyvsp[(7) - (7)].str));
                 }
    break;

  case 56:
#line 2084 "parser.y"
    {
		   Hash *p = (yyvsp[(5) - (7)].node);
		   String *code;
                   skip_balanced('{','}');
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[(3) - (7)].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[(3) - (7)].node),"type"));
		   Setattr((yyval.node),"section",Getattr(p,"name"));
		   Setattr((yyval.node),"kwargs",nextSibling(p));
		   Delitem(scanner_ccode,0);
		   Delitem(scanner_ccode,DOH_END);
		   code = Copy(scanner_ccode);
		   Setattr((yyval.node),"code",code);
		   Delete(code);
                 }
    break;

  case 57:
#line 2099 "parser.y"
    {
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[(3) - (5)].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[(3) - (5)].node),"type"));
		   Setattr((yyval.node),"emitonly","1");
		 }
    break;

  case 58:
#line 2112 "parser.y"
    {
                     (yyvsp[(1) - (4)].loc).filename = Copy(cparse_file);
		     (yyvsp[(1) - (4)].loc).line = cparse_line;
		     scanner_set_location(NewString((yyvsp[(3) - (4)].id)),1);
                     if ((yyvsp[(2) - (4)].node)) { 
		       String *maininput = Getattr((yyvsp[(2) - (4)].node), "maininput");
		       if (maininput)
		         scanner_set_main_input_file(NewString(maininput));
		     }
               }
    break;

  case 59:
#line 2121 "parser.y"
    {
                     String *mname = 0;
                     (yyval.node) = (yyvsp[(6) - (7)].node);
		     scanner_set_location((yyvsp[(1) - (7)].loc).filename,(yyvsp[(1) - (7)].loc).line);
		     if (strcmp((yyvsp[(1) - (7)].loc).type,"include") == 0) set_nodeType((yyval.node),"include");
		     if (strcmp((yyvsp[(1) - (7)].loc).type,"import") == 0) {
		       mname = (yyvsp[(2) - (7)].node) ? Getattr((yyvsp[(2) - (7)].node),"module") : 0;
		       set_nodeType((yyval.node),"import");
		       if (import_mode) --import_mode;
		     }
		     
		     Setattr((yyval.node),"name",(yyvsp[(3) - (7)].id));
		     /* Search for the module (if any) */
		     {
			 Node *n = firstChild((yyval.node));
			 while (n) {
			     if (Strcmp(nodeType(n),"module") == 0) {
			         if (mname) {
				   Setattr(n,"name", mname);
				   mname = 0;
				 }
				 Setattr((yyval.node),"module",Getattr(n,"name"));
				 break;
			     }
			     n = nextSibling(n);
			 }
			 if (mname) {
			   /* There is no module node in the import
			      node, ie, you imported a .h file
			      directly.  We are forced then to create
			      a new import node with a module node.
			   */			      
			   Node *nint = new_node("import");
			   Node *mnode = new_node("module");
			   Setattr(mnode,"name", mname);
			   appendChild(nint,mnode);
			   Delete(mnode);
			   appendChild(nint,firstChild((yyval.node)));
			   (yyval.node) = nint;
			   Setattr((yyval.node),"module",mname);
			 }
		     }
		     Setattr((yyval.node),"options",(yyvsp[(2) - (7)].node));
               }
    break;

  case 60:
#line 2167 "parser.y"
    { (yyval.loc).type = (char *) "include"; }
    break;

  case 61:
#line 2168 "parser.y"
    { (yyval.loc).type = (char *) "import"; ++import_mode;}
    break;

  case 62:
#line 2175 "parser.y"
    {
                 String *cpps;
		 if (Namespaceprefix) {
		   Swig_error(cparse_file, cparse_start_line, "%%inline directive inside a namespace is disallowed.\n");

		   (yyval.node) = 0;
		 } else {
		   (yyval.node) = new_node("insert");
		   Setattr((yyval.node),"code",(yyvsp[(2) - (2)].str));
		   /* Need to run through the preprocessor */
		   Setline((yyvsp[(2) - (2)].str),cparse_start_line);
		   Setfile((yyvsp[(2) - (2)].str),cparse_file);
		   Seek((yyvsp[(2) - (2)].str),0,SEEK_SET);
		   cpps = Preprocessor_parse((yyvsp[(2) - (2)].str));
		   start_inline(Char(cpps), cparse_start_line);
		   Delete((yyvsp[(2) - (2)].str));
		   Delete(cpps);
		 }
		 
	       }
    break;

  case 63:
#line 2195 "parser.y"
    {
                 String *cpps;
		 int start_line = cparse_line;
		 skip_balanced('{','}');
		 if (Namespaceprefix) {
		   Swig_error(cparse_file, cparse_start_line, "%%inline directive inside a namespace is disallowed.\n");
		   
		   (yyval.node) = 0;
		 } else {
		   String *code;
                   (yyval.node) = new_node("insert");
		   Delitem(scanner_ccode,0);
		   Delitem(scanner_ccode,DOH_END);
		   code = Copy(scanner_ccode);
		   Setattr((yyval.node),"code", code);
		   Delete(code);		   
		   cpps=Copy(scanner_ccode);
		   start_inline(Char(cpps), start_line);
		   Delete(cpps);
		 }
               }
    break;

  case 64:
#line 2226 "parser.y"
    {
                 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"code",(yyvsp[(1) - (1)].str));
	       }
    break;

  case 65:
#line 2230 "parser.y"
    {
		 String *code = NewStringEmpty();
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[(3) - (5)].id));
		 Setattr((yyval.node),"code",code);
		 if (Swig_insert_file((yyvsp[(5) - (5)].id),code) < 0) {
		   Swig_error(cparse_file, cparse_line, "Couldn't find '%s'.\n", (yyvsp[(5) - (5)].id));
		   (yyval.node) = 0;
		 } 
               }
    break;

  case 66:
#line 2240 "parser.y"
    {
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[(3) - (5)].id));
		 Setattr((yyval.node),"code",(yyvsp[(5) - (5)].str));
               }
    break;

  case 67:
#line 2245 "parser.y"
    {
		 String *code;
                 skip_balanced('{','}');
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[(3) - (5)].id));
		 Delitem(scanner_ccode,0);
		 Delitem(scanner_ccode,DOH_END);
		 code = Copy(scanner_ccode);
		 Setattr((yyval.node),"code", code);
		 Delete(code);
	       }
    break;

  case 68:
#line 2263 "parser.y"
    {
                 (yyval.node) = new_node("module");
		 if ((yyvsp[(2) - (3)].node)) {
		   Setattr((yyval.node),"options",(yyvsp[(2) - (3)].node));
		   if (Getattr((yyvsp[(2) - (3)].node),"directors")) {
		     Wrapper_director_mode_set(1);
		   } 
		   if (Getattr((yyvsp[(2) - (3)].node),"dirprot")) {
		     Wrapper_director_protected_mode_set(1);
		   } 
		   if (Getattr((yyvsp[(2) - (3)].node),"allprotected")) {
		     Wrapper_all_protected_mode_set(1);
		   } 
		   if (Getattr((yyvsp[(2) - (3)].node),"templatereduce")) {
		     template_reduce = 1;
		   }
		   if (Getattr((yyvsp[(2) - (3)].node),"notemplatereduce")) {
		     template_reduce = 0;
		   }
		 }
		 if (!ModuleName) ModuleName = NewString((yyvsp[(3) - (3)].id));
		 if (!import_mode) {
		   /* first module included, we apply global
		      ModuleName, which can be modify by -module */
		   String *mname = Copy(ModuleName);
		   Setattr((yyval.node),"name",mname);
		   Delete(mname);
		 } else { 
		   /* import mode, we just pass the idstring */
		   Setattr((yyval.node),"name",(yyvsp[(3) - (3)].id));   
		 }		 
		 if (!module_node) module_node = (yyval.node);
	       }
    break;

  case 69:
#line 2303 "parser.y"
    {
                 Swig_warning(WARN_DEPRECATED_NAME,cparse_file,cparse_line, "%%name is deprecated.  Use %%rename instead.\n");
		 Delete(yyrename);
                 yyrename = NewString((yyvsp[(3) - (4)].id));
		 (yyval.node) = 0;
               }
    break;

  case 70:
#line 2309 "parser.y"
    {
		 Swig_warning(WARN_DEPRECATED_NAME,cparse_file,cparse_line, "%%name is deprecated.  Use %%rename instead.\n");
		 (yyval.node) = 0;
		 Swig_error(cparse_file,cparse_line,"Missing argument to %%name directive.\n");
	       }
    break;

  case 71:
#line 2322 "parser.y"
    {
                 (yyval.node) = new_node("native");
		 Setattr((yyval.node),"name",(yyvsp[(3) - (7)].id));
		 Setattr((yyval.node),"wrap:name",(yyvsp[(6) - (7)].id));
	         add_symbols((yyval.node));
	       }
    break;

  case 72:
#line 2328 "parser.y"
    {
		 if (!SwigType_isfunction((yyvsp[(7) - (8)].decl).type)) {
		   Swig_error(cparse_file,cparse_line,"%%native declaration '%s' is not a function.\n", (yyvsp[(7) - (8)].decl).id);
		   (yyval.node) = 0;
		 } else {
		     Delete(SwigType_pop_function((yyvsp[(7) - (8)].decl).type));
		     /* Need check for function here */
		     SwigType_push((yyvsp[(6) - (8)].type),(yyvsp[(7) - (8)].decl).type);
		     (yyval.node) = new_node("native");
	             Setattr((yyval.node),"name",(yyvsp[(3) - (8)].id));
		     Setattr((yyval.node),"wrap:name",(yyvsp[(7) - (8)].decl).id);
		     Setattr((yyval.node),"type",(yyvsp[(6) - (8)].type));
		     Setattr((yyval.node),"parms",(yyvsp[(7) - (8)].decl).parms);
		     Setattr((yyval.node),"decl",(yyvsp[(7) - (8)].decl).type);
		 }
	         add_symbols((yyval.node));
	       }
    break;

  case 73:
#line 2354 "parser.y"
    {
                 (yyval.node) = new_node("pragma");
		 Setattr((yyval.node),"lang",(yyvsp[(2) - (5)].id));
		 Setattr((yyval.node),"name",(yyvsp[(3) - (5)].id));
		 Setattr((yyval.node),"value",(yyvsp[(5) - (5)].str));
	       }
    break;

  case 74:
#line 2360 "parser.y"
    {
		(yyval.node) = new_node("pragma");
		Setattr((yyval.node),"lang",(yyvsp[(2) - (3)].id));
		Setattr((yyval.node),"name",(yyvsp[(3) - (3)].id));
	      }
    break;

  case 75:
#line 2367 "parser.y"
    { (yyval.str) = NewString((yyvsp[(1) - (1)].id)); }
    break;

  case 76:
#line 2368 "parser.y"
    { (yyval.str) = (yyvsp[(1) - (1)].str); }
    break;

  case 77:
#line 2371 "parser.y"
    { (yyval.id) = (yyvsp[(2) - (3)].id); }
    break;

  case 78:
#line 2372 "parser.y"
    { (yyval.id) = (char *) "swig"; }
    break;

  case 79:
#line 2380 "parser.y"
    {
                SwigType *t = (yyvsp[(2) - (4)].decl).type;
		Hash *kws = NewHash();
		String *fixname;
		fixname = feature_identifier_fix((yyvsp[(2) - (4)].decl).id);
		Setattr(kws,"name",(yyvsp[(3) - (4)].id));
		if (!Len(t)) t = 0;
		/* Special declarator check */
		if (t) {
		  if (SwigType_isfunction(t)) {
		    SwigType *decl = SwigType_pop_function(t);
		    if (SwigType_ispointer(t)) {
		      String *nname = NewStringf("*%s",fixname);
		      if ((yyvsp[(1) - (4)].ivalue)) {
			Swig_name_rename_add(Namespaceprefix, nname,decl,kws,(yyvsp[(2) - (4)].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,nname,decl,kws);
		      }
		      Delete(nname);
		    } else {
		      if ((yyvsp[(1) - (4)].ivalue)) {
			Swig_name_rename_add(Namespaceprefix,(fixname),decl,kws,(yyvsp[(2) - (4)].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,(fixname),decl,kws);
		      }
		    }
		    Delete(decl);
		  } else if (SwigType_ispointer(t)) {
		    String *nname = NewStringf("*%s",fixname);
		    if ((yyvsp[(1) - (4)].ivalue)) {
		      Swig_name_rename_add(Namespaceprefix,(nname),0,kws,(yyvsp[(2) - (4)].decl).parms);
		    } else {
		      Swig_name_namewarn_add(Namespaceprefix,(nname),0,kws);
		    }
		    Delete(nname);
		  }
		} else {
		  if ((yyvsp[(1) - (4)].ivalue)) {
		    Swig_name_rename_add(Namespaceprefix,(fixname),0,kws,(yyvsp[(2) - (4)].decl).parms);
		  } else {
		    Swig_name_namewarn_add(Namespaceprefix,(fixname),0,kws);
		  }
		}
                (yyval.node) = 0;
		scanner_clear_rename();
              }
    break;

  case 80:
#line 2426 "parser.y"
    {
		String *fixname;
		Hash *kws = (yyvsp[(3) - (7)].node);
		SwigType *t = (yyvsp[(5) - (7)].decl).type;
		fixname = feature_identifier_fix((yyvsp[(5) - (7)].decl).id);
		if (!Len(t)) t = 0;
		/* Special declarator check */
		if (t) {
		  if ((yyvsp[(6) - (7)].dtype).qualifier) SwigType_push(t,(yyvsp[(6) - (7)].dtype).qualifier);
		  if (SwigType_isfunction(t)) {
		    SwigType *decl = SwigType_pop_function(t);
		    if (SwigType_ispointer(t)) {
		      String *nname = NewStringf("*%s",fixname);
		      if ((yyvsp[(1) - (7)].ivalue)) {
			Swig_name_rename_add(Namespaceprefix, nname,decl,kws,(yyvsp[(5) - (7)].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,nname,decl,kws);
		      }
		      Delete(nname);
		    } else {
		      if ((yyvsp[(1) - (7)].ivalue)) {
			Swig_name_rename_add(Namespaceprefix,(fixname),decl,kws,(yyvsp[(5) - (7)].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,(fixname),decl,kws);
		      }
		    }
		    Delete(decl);
		  } else if (SwigType_ispointer(t)) {
		    String *nname = NewStringf("*%s",fixname);
		    if ((yyvsp[(1) - (7)].ivalue)) {
		      Swig_name_rename_add(Namespaceprefix,(nname),0,kws,(yyvsp[(5) - (7)].decl).parms);
		    } else {
		      Swig_name_namewarn_add(Namespaceprefix,(nname),0,kws);
		    }
		    Delete(nname);
		  }
		} else {
		  if ((yyvsp[(1) - (7)].ivalue)) {
		    Swig_name_rename_add(Namespaceprefix,(fixname),0,kws,(yyvsp[(5) - (7)].decl).parms);
		  } else {
		    Swig_name_namewarn_add(Namespaceprefix,(fixname),0,kws);
		  }
		}
                (yyval.node) = 0;
		scanner_clear_rename();
              }
    break;

  case 81:
#line 2472 "parser.y"
    {
		if ((yyvsp[(1) - (6)].ivalue)) {
		  Swig_name_rename_add(Namespaceprefix,(yyvsp[(5) - (6)].id),0,(yyvsp[(3) - (6)].node),0);
		} else {
		  Swig_name_namewarn_add(Namespaceprefix,(yyvsp[(5) - (6)].id),0,(yyvsp[(3) - (6)].node));
		}
		(yyval.node) = 0;
		scanner_clear_rename();
              }
    break;

  case 82:
#line 2483 "parser.y"
    {
		    (yyval.ivalue) = 1;
                }
    break;

  case 83:
#line 2486 "parser.y"
    {
                    (yyval.ivalue) = 0;
                }
    break;

  case 84:
#line 2513 "parser.y"
    {
                    String *val = (yyvsp[(7) - (7)].str) ? NewString((yyvsp[(7) - (7)].str)) : NewString("1");
                    new_feature((yyvsp[(3) - (7)].id), val, 0, (yyvsp[(5) - (7)].decl).id, (yyvsp[(5) - (7)].decl).type, (yyvsp[(5) - (7)].decl).parms, (yyvsp[(6) - (7)].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 85:
#line 2519 "parser.y"
    {
                    String *val = Len((yyvsp[(5) - (9)].id)) ? NewString((yyvsp[(5) - (9)].id)) : 0;
                    new_feature((yyvsp[(3) - (9)].id), val, 0, (yyvsp[(7) - (9)].decl).id, (yyvsp[(7) - (9)].decl).type, (yyvsp[(7) - (9)].decl).parms, (yyvsp[(8) - (9)].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 86:
#line 2525 "parser.y"
    {
                    String *val = (yyvsp[(8) - (8)].str) ? NewString((yyvsp[(8) - (8)].str)) : NewString("1");
                    new_feature((yyvsp[(3) - (8)].id), val, (yyvsp[(4) - (8)].node), (yyvsp[(6) - (8)].decl).id, (yyvsp[(6) - (8)].decl).type, (yyvsp[(6) - (8)].decl).parms, (yyvsp[(7) - (8)].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 87:
#line 2531 "parser.y"
    {
                    String *val = Len((yyvsp[(5) - (10)].id)) ? NewString((yyvsp[(5) - (10)].id)) : 0;
                    new_feature((yyvsp[(3) - (10)].id), val, (yyvsp[(6) - (10)].node), (yyvsp[(8) - (10)].decl).id, (yyvsp[(8) - (10)].decl).type, (yyvsp[(8) - (10)].decl).parms, (yyvsp[(9) - (10)].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 88:
#line 2539 "parser.y"
    {
                    String *val = (yyvsp[(5) - (5)].str) ? NewString((yyvsp[(5) - (5)].str)) : NewString("1");
                    new_feature((yyvsp[(3) - (5)].id), val, 0, 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 89:
#line 2545 "parser.y"
    {
                    String *val = Len((yyvsp[(5) - (7)].id)) ? NewString((yyvsp[(5) - (7)].id)) : 0;
                    new_feature((yyvsp[(3) - (7)].id), val, 0, 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 90:
#line 2551 "parser.y"
    {
                    String *val = (yyvsp[(6) - (6)].str) ? NewString((yyvsp[(6) - (6)].str)) : NewString("1");
                    new_feature((yyvsp[(3) - (6)].id), val, (yyvsp[(4) - (6)].node), 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 91:
#line 2557 "parser.y"
    {
                    String *val = Len((yyvsp[(5) - (8)].id)) ? NewString((yyvsp[(5) - (8)].id)) : 0;
                    new_feature((yyvsp[(3) - (8)].id), val, (yyvsp[(6) - (8)].node), 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
    break;

  case 92:
#line 2565 "parser.y"
    { (yyval.str) = (yyvsp[(1) - (1)].str); }
    break;

  case 93:
#line 2566 "parser.y"
    { (yyval.str) = 0; }
    break;

  case 94:
#line 2567 "parser.y"
    { (yyval.str) = (yyvsp[(3) - (5)].pl); }
    break;

  case 95:
#line 2570 "parser.y"
    {
		  (yyval.node) = NewHash();
		  Setattr((yyval.node),"name",(yyvsp[(2) - (4)].id));
		  Setattr((yyval.node),"value",(yyvsp[(4) - (4)].id));
                }
    break;

  case 96:
#line 2575 "parser.y"
    {
		  (yyval.node) = NewHash();
		  Setattr((yyval.node),"name",(yyvsp[(2) - (5)].id));
		  Setattr((yyval.node),"value",(yyvsp[(4) - (5)].id));
                  set_nextSibling((yyval.node),(yyvsp[(5) - (5)].node));
                }
    break;

  case 97:
#line 2585 "parser.y"
    {
                 Parm *val;
		 String *name;
		 SwigType *t;
		 if (Namespaceprefix) name = NewStringf("%s::%s", Namespaceprefix, (yyvsp[(5) - (7)].decl).id);
		 else name = NewString((yyvsp[(5) - (7)].decl).id);
		 val = (yyvsp[(3) - (7)].pl);
		 if ((yyvsp[(5) - (7)].decl).parms) {
		   Setmeta(val,"parms",(yyvsp[(5) - (7)].decl).parms);
		 }
		 t = (yyvsp[(5) - (7)].decl).type;
		 if (!Len(t)) t = 0;
		 if (t) {
		   if ((yyvsp[(6) - (7)].dtype).qualifier) SwigType_push(t,(yyvsp[(6) - (7)].dtype).qualifier);
		   if (SwigType_isfunction(t)) {
		     SwigType *decl = SwigType_pop_function(t);
		     if (SwigType_ispointer(t)) {
		       String *nname = NewStringf("*%s",name);
		       Swig_feature_set(Swig_cparse_features(), nname, decl, "feature:varargs", val, 0);
		       Delete(nname);
		     } else {
		       Swig_feature_set(Swig_cparse_features(), name, decl, "feature:varargs", val, 0);
		     }
		     Delete(decl);
		   } else if (SwigType_ispointer(t)) {
		     String *nname = NewStringf("*%s",name);
		     Swig_feature_set(Swig_cparse_features(),nname,0,"feature:varargs",val, 0);
		     Delete(nname);
		   }
		 } else {
		   Swig_feature_set(Swig_cparse_features(),name,0,"feature:varargs",val, 0);
		 }
		 Delete(name);
		 (yyval.node) = 0;
              }
    break;

  case 98:
#line 2621 "parser.y"
    { (yyval.pl) = (yyvsp[(1) - (1)].pl); }
    break;

  case 99:
#line 2622 "parser.y"
    { 
		  int i;
		  int n;
		  Parm *p;
		  n = atoi(Char((yyvsp[(1) - (3)].dtype).val));
		  if (n <= 0) {
		    Swig_error(cparse_file, cparse_line,"Argument count in %%varargs must be positive.\n");
		    (yyval.pl) = 0;
		  } else {
		    (yyval.pl) = Copy((yyvsp[(3) - (3)].p));
		    Setattr((yyval.pl),"name","VARARGS_SENTINEL");
		    for (i = 0; i < n; i++) {
		      p = Copy((yyvsp[(3) - (3)].p));
		      set_nextSibling(p,(yyval.pl));
		      Delete((yyval.pl));
		      (yyval.pl) = p;
		    }
		  }
                }
    break;

  case 100:
#line 2652 "parser.y"
    {
		   (yyval.node) = 0;
		   if ((yyvsp[(3) - (6)].tmap).method) {
		     String *code = 0;
		     (yyval.node) = new_node("typemap");
		     Setattr((yyval.node),"method",(yyvsp[(3) - (6)].tmap).method);
		     if ((yyvsp[(3) - (6)].tmap).kwargs) {
		       ParmList *kw = (yyvsp[(3) - (6)].tmap).kwargs;
                       code = remove_block(kw, (yyvsp[(6) - (6)].str));
		       Setattr((yyval.node),"kwargs", (yyvsp[(3) - (6)].tmap).kwargs);
		     }
		     code = code ? code : NewString((yyvsp[(6) - (6)].str));
		     Setattr((yyval.node),"code", code);
		     Delete(code);
		     appendChild((yyval.node),(yyvsp[(5) - (6)].p));
		   }
	       }
    break;

  case 101:
#line 2669 "parser.y"
    {
		 (yyval.node) = 0;
		 if ((yyvsp[(3) - (6)].tmap).method) {
		   (yyval.node) = new_node("typemap");
		   Setattr((yyval.node),"method",(yyvsp[(3) - (6)].tmap).method);
		   appendChild((yyval.node),(yyvsp[(5) - (6)].p));
		 }
	       }
    break;

  case 102:
#line 2677 "parser.y"
    {
		   (yyval.node) = 0;
		   if ((yyvsp[(3) - (8)].tmap).method) {
		     (yyval.node) = new_node("typemapcopy");
		     Setattr((yyval.node),"method",(yyvsp[(3) - (8)].tmap).method);
		     Setattr((yyval.node),"pattern", Getattr((yyvsp[(7) - (8)].p),"pattern"));
		     appendChild((yyval.node),(yyvsp[(5) - (8)].p));
		   }
	       }
    break;

  case 103:
#line 2690 "parser.y"
    {
		 Hash *p;
		 String *name;
		 p = nextSibling((yyvsp[(1) - (1)].node));
		 if (p && (!Getattr(p,"value"))) {
 		   /* this is the deprecated two argument typemap form */
 		   Swig_warning(WARN_DEPRECATED_TYPEMAP_LANG,cparse_file, cparse_line,
				"Specifying the language name in %%typemap is deprecated - use #ifdef SWIG<LANG> instead.\n");
		   /* two argument typemap form */
		   name = Getattr((yyvsp[(1) - (1)].node),"name");
		   if (!name || (Strcmp(name,typemap_lang))) {
		     (yyval.tmap).method = 0;
		     (yyval.tmap).kwargs = 0;
		   } else {
		     (yyval.tmap).method = Getattr(p,"name");
		     (yyval.tmap).kwargs = nextSibling(p);
		   }
		 } else {
		   /* one-argument typemap-form */
		   (yyval.tmap).method = Getattr((yyvsp[(1) - (1)].node),"name");
		   (yyval.tmap).kwargs = p;
		 }
                }
    break;

  case 104:
#line 2715 "parser.y"
    {
                 (yyval.p) = (yyvsp[(1) - (2)].p);
		 set_nextSibling((yyval.p),(yyvsp[(2) - (2)].p));
		}
    break;

  case 105:
#line 2721 "parser.y"
    {
                 (yyval.p) = (yyvsp[(2) - (3)].p);
		 set_nextSibling((yyval.p),(yyvsp[(3) - (3)].p));
                }
    break;

  case 106:
#line 2725 "parser.y"
    { (yyval.p) = 0;}
    break;

  case 107:
#line 2728 "parser.y"
    {
                  Parm *parm;
		  SwigType_push((yyvsp[(1) - (2)].type),(yyvsp[(2) - (2)].decl).type);
		  (yyval.p) = new_node("typemapitem");
		  parm = NewParmWithoutFileLineInfo((yyvsp[(1) - (2)].type),(yyvsp[(2) - (2)].decl).id);
		  Setattr((yyval.p),"pattern",parm);
		  Setattr((yyval.p),"parms", (yyvsp[(2) - (2)].decl).parms);
		  Delete(parm);
		  /*		  $$ = NewParmWithoutFileLineInfo($1,$2.id);
				  Setattr($$,"parms",$2.parms); */
                }
    break;

  case 108:
#line 2739 "parser.y"
    {
                  (yyval.p) = new_node("typemapitem");
		  Setattr((yyval.p),"pattern",(yyvsp[(2) - (3)].pl));
		  /*		  Setattr($$,"multitype",$2); */
               }
    break;

  case 109:
#line 2744 "parser.y"
    {
		 (yyval.p) = new_node("typemapitem");
		 Setattr((yyval.p),"pattern", (yyvsp[(2) - (6)].pl));
		 /*                 Setattr($$,"multitype",$2); */
		 Setattr((yyval.p),"parms",(yyvsp[(5) - (6)].pl));
               }
    break;

  case 110:
#line 2757 "parser.y"
    {
                   (yyval.node) = new_node("types");
		   Setattr((yyval.node),"parms",(yyvsp[(3) - (5)].pl));
                   if ((yyvsp[(5) - (5)].str))
		     Setattr((yyval.node),"convcode",NewString((yyvsp[(5) - (5)].str)));
               }
    break;

  case 111:
#line 2769 "parser.y"
    {
                  Parm *p, *tp;
		  Node *n;
		  Node *tnode = 0;
		  Symtab *tscope = 0;
		  int     specialized = 0;

		  (yyval.node) = 0;

		  tscope = Swig_symbol_current();          /* Get the current scope */

		  /* If the class name is qualified, we need to create or lookup namespace entries */
		  if (!inclass) {
		    (yyvsp[(5) - (9)].str) = resolve_node_scope((yyvsp[(5) - (9)].str));
		  }

		  /*
		    We use the new namespace entry 'nscope' only to
		    emit the template node. The template parameters are
		    resolved in the current 'tscope'.

		    This is closer to the C++ (typedef) behavior.
		  */
		  n = Swig_cparse_template_locate((yyvsp[(5) - (9)].str),(yyvsp[(7) - (9)].p),tscope);

		  /* Patch the argument types to respect namespaces */
		  p = (yyvsp[(7) - (9)].p);
		  while (p) {
		    SwigType *value = Getattr(p,"value");
		    if (!value) {
		      SwigType *ty = Getattr(p,"type");
		      if (ty) {
			SwigType *rty = 0;
			int reduce = template_reduce;
			if (reduce || !SwigType_ispointer(ty)) {
			  rty = Swig_symbol_typedef_reduce(ty,tscope);
			  if (!reduce) reduce = SwigType_ispointer(rty);
			}
			ty = reduce ? Swig_symbol_type_qualify(rty,tscope) : Swig_symbol_type_qualify(ty,tscope);
			Setattr(p,"type",ty);
			Delete(ty);
			Delete(rty);
		      }
		    } else {
		      value = Swig_symbol_type_qualify(value,tscope);
		      Setattr(p,"value",value);
		      Delete(value);
		    }

		    p = nextSibling(p);
		  }

		  /* Look for the template */
		  {
                    Node *nn = n;
                    Node *linklistend = 0;
                    while (nn) {
                      Node *templnode = 0;
                      if (Strcmp(nodeType(nn),"template") == 0) {
                        int nnisclass = (Strcmp(Getattr(nn,"templatetype"),"class") == 0); /* if not a templated class it is a templated function */
                        Parm *tparms = Getattr(nn,"templateparms");
                        if (!tparms) {
                          specialized = 1;
                        }
                        if (nnisclass && !specialized && ((ParmList_len((yyvsp[(7) - (9)].p)) > ParmList_len(tparms)))) {
                          Swig_error(cparse_file, cparse_line, "Too many template parameters. Maximum of %d.\n", ParmList_len(tparms));
                        } else if (nnisclass && !specialized && ((ParmList_len((yyvsp[(7) - (9)].p)) < ParmList_numrequired(tparms)))) {
                          Swig_error(cparse_file, cparse_line, "Not enough template parameters specified. %d required.\n", ParmList_numrequired(tparms));
                        } else if (!nnisclass && ((ParmList_len((yyvsp[(7) - (9)].p)) != ParmList_len(tparms)))) {
                          /* must be an overloaded templated method - ignore it as it is overloaded with a different number of template parameters */
                          nn = Getattr(nn,"sym:nextSibling"); /* repeat for overloaded templated functions */
                          continue;
                        } else {
			  String *tname = Copy((yyvsp[(5) - (9)].str));
                          int def_supplied = 0;
                          /* Expand the template */
			  Node *templ = Swig_symbol_clookup((yyvsp[(5) - (9)].str),0);
			  Parm *targs = templ ? Getattr(templ,"templateparms") : 0;

                          ParmList *temparms;
                          if (specialized) temparms = CopyParmList((yyvsp[(7) - (9)].p));
                          else temparms = CopyParmList(tparms);

                          /* Create typedef's and arguments */
                          p = (yyvsp[(7) - (9)].p);
                          tp = temparms;
                          if (!p && ParmList_len(p) != ParmList_len(temparms)) {
                            /* we have no template parameters supplied in %template for a template that has default args*/
                            p = tp;
                            def_supplied = 1;
                          }

                          while (p) {
                            String *value = Getattr(p,"value");
                            if (def_supplied) {
                              Setattr(p,"default","1");
                            }
                            if (value) {
                              Setattr(tp,"value",value);
                            } else {
                              SwigType *ty = Getattr(p,"type");
                              if (ty) {
                                Setattr(tp,"type",ty);
                              }
                              Delattr(tp,"value");
                            }
			    /* fix default arg values */
			    if (targs) {
			      Parm *pi = temparms;
			      Parm *ti = targs;
			      String *tv = Getattr(tp,"value");
			      if (!tv) tv = Getattr(tp,"type");
			      while(pi != tp && ti && pi) {
				String *name = Getattr(ti,"name");
				String *value = Getattr(pi,"value");
				if (!value) value = Getattr(pi,"type");
				Replaceid(tv, name, value);
				pi = nextSibling(pi);
				ti = nextSibling(ti);
			      }
			    }
                            p = nextSibling(p);
                            tp = nextSibling(tp);
                            if (!p && tp) {
                              p = tp;
                              def_supplied = 1;
                            }
                          }

                          templnode = copy_node(nn);
                          /* We need to set the node name based on name used to instantiate */
                          Setattr(templnode,"name",tname);
			  Delete(tname);
                          if (!specialized) {
                            Delattr(templnode,"sym:typename");
                          } else {
                            Setattr(templnode,"sym:typename","1");
                          }
                          if ((yyvsp[(3) - (9)].id) && !inclass) {
			    /*
			       Comment this out for 1.3.28. We need to
			       re-enable it later but first we need to
			       move %ignore from using %rename to use
			       %feature(ignore).

			       String *symname = Swig_name_make(templnode,0,$3,0,0);
			    */
			    String *symname = (yyvsp[(3) - (9)].id);
                            Swig_cparse_template_expand(templnode,symname,temparms,tscope);
                            Setattr(templnode,"sym:name",symname);
                          } else {
                            static int cnt = 0;
                            String *nname = NewStringf("__dummy_%d__", cnt++);
                            Swig_cparse_template_expand(templnode,nname,temparms,tscope);
                            Setattr(templnode,"sym:name",nname);
			    Delete(nname);
                            Setattr(templnode,"feature:onlychildren", "typemap,typemapitem,typemapcopy,typedef,types,fragment");

			    if ((yyvsp[(3) - (9)].id)) {
			      Swig_warning(WARN_PARSE_NESTED_TEMPLATE, cparse_file, cparse_line, "Named nested template instantiations not supported. Processing as if no name was given to %%template().\n");
			    }
                          }
                          Delattr(templnode,"templatetype");
                          Setattr(templnode,"template",nn);
                          tnode = templnode;
                          Setfile(templnode,cparse_file);
                          Setline(templnode,cparse_line);
                          Delete(temparms);

                          add_symbols_copy(templnode);

                          if (Strcmp(nodeType(templnode),"class") == 0) {

                            /* Identify pure abstract methods */
                            Setattr(templnode,"abstract", pure_abstract(firstChild(templnode)));

                            /* Set up inheritance in symbol table */
                            {
                              Symtab  *csyms;
                              List *baselist = Getattr(templnode,"baselist");
                              csyms = Swig_symbol_current();
                              Swig_symbol_setscope(Getattr(templnode,"symtab"));
                              if (baselist) {
                                List *bases = make_inherit_list(Getattr(templnode,"name"),baselist);
                                if (bases) {
                                  Iterator s;
                                  for (s = First(bases); s.item; s = Next(s)) {
                                    Symtab *st = Getattr(s.item,"symtab");
                                    if (st) {
				      Setfile(st,Getfile(s.item));
				      Setline(st,Getline(s.item));
                                      Swig_symbol_inherit(st);
                                    }
                                  }
				  Delete(bases);
                                }
                              }
                              Swig_symbol_setscope(csyms);
                            }

                            /* Merge in %extend methods for this class */

			    /* !!! This may be broken.  We may have to add the
			       %extend methods at the beginning of the class */

                            if (extendhash) {
                              String *stmp = 0;
                              String *clsname;
                              Node *am;
                              if (Namespaceprefix) {
                                clsname = stmp = NewStringf("%s::%s", Namespaceprefix, Getattr(templnode,"name"));
                              } else {
                                clsname = Getattr(templnode,"name");
                              }
                              am = Getattr(extendhash,clsname);
                              if (am) {
                                Symtab *st = Swig_symbol_current();
                                Swig_symbol_setscope(Getattr(templnode,"symtab"));
                                /*			    Printf(stdout,"%s: %s %x %x\n", Getattr(templnode,"name"), clsname, Swig_symbol_current(), Getattr(templnode,"symtab")); */
                                merge_extensions(templnode,am);
                                Swig_symbol_setscope(st);
				append_previous_extension(templnode,am);
                                Delattr(extendhash,clsname);
                              }
			      if (stmp) Delete(stmp);
                            }
                            /* Add to classes hash */
                            if (!classes) classes = NewHash();

                            {
                              if (Namespaceprefix) {
                                String *temp = NewStringf("%s::%s", Namespaceprefix, Getattr(templnode,"name"));
                                Setattr(classes,temp,templnode);
				Delete(temp);
                              } else {
				String *qs = Swig_symbol_qualifiedscopename(templnode);
                                Setattr(classes, qs,templnode);
				Delete(qs);
                              }
                            }
                          }
                        }

                        /* all the overloaded templated functions are added into a linked list */
                        if (nscope_inner) {
                          /* non-global namespace */
                          if (templnode) {
                            appendChild(nscope_inner,templnode);
			    Delete(templnode);
                            if (nscope) (yyval.node) = nscope;
                          }
                        } else {
                          /* global namespace */
                          if (!linklistend) {
                            (yyval.node) = templnode;
                          } else {
                            set_nextSibling(linklistend,templnode);
			    Delete(templnode);
                          }
                          linklistend = templnode;
                        }
                      }
                      nn = Getattr(nn,"sym:nextSibling"); /* repeat for overloaded templated functions. If a templated class there will never be a sibling. */
                    }
		  }
	          Swig_symbol_setscope(tscope);
		  Delete(Namespaceprefix);
		  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
                }
    break;

  case 112:
#line 3045 "parser.y"
    {
		  Swig_warning(0,cparse_file, cparse_line,"%s\n", (yyvsp[(2) - (2)].id));
		  (yyval.node) = 0;
               }
    break;

  case 113:
#line 3055 "parser.y"
    {
                    (yyval.node) = (yyvsp[(1) - (1)].node); 
                    if ((yyval.node)) {
   		      add_symbols((yyval.node));
                      default_arguments((yyval.node));
   	            }
                }
    break;

  case 114:
#line 3062 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 115:
#line 3063 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 116:
#line 3067 "parser.y"
    {
		  if (Strcmp((yyvsp[(2) - (3)].id),"C") == 0) {
		    cparse_externc = 1;
		  }
		}
    break;

  case 117:
#line 3071 "parser.y"
    {
		  cparse_externc = 0;
		  if (Strcmp((yyvsp[(2) - (6)].id),"C") == 0) {
		    Node *n = firstChild((yyvsp[(5) - (6)].node));
		    (yyval.node) = new_node("extern");
		    Setattr((yyval.node),"name",(yyvsp[(2) - (6)].id));
		    appendChild((yyval.node),n);
		    while (n) {
		      SwigType *decl = Getattr(n,"decl");
		      if (SwigType_isfunction(decl) && Strcmp(Getattr(n, "storage"), "typedef") != 0) {
			Setattr(n,"storage","externc");
		      }
		      n = nextSibling(n);
		    }
		  } else {
		     Swig_warning(WARN_PARSE_UNDEFINED_EXTERN,cparse_file, cparse_line,"Unrecognized extern type \"%s\".\n", (yyvsp[(2) - (6)].id));
		    (yyval.node) = new_node("extern");
		    Setattr((yyval.node),"name",(yyvsp[(2) - (6)].id));
		    appendChild((yyval.node),firstChild((yyvsp[(5) - (6)].node)));
		  }
                }
    break;

  case 118:
#line 3098 "parser.y"
    {
              (yyval.node) = new_node("cdecl");
	      if ((yyvsp[(4) - (5)].dtype).qualifier) SwigType_push((yyvsp[(3) - (5)].decl).type,(yyvsp[(4) - (5)].dtype).qualifier);
	      Setattr((yyval.node),"type",(yyvsp[(2) - (5)].type));
	      Setattr((yyval.node),"storage",(yyvsp[(1) - (5)].id));
	      Setattr((yyval.node),"name",(yyvsp[(3) - (5)].decl).id);
	      Setattr((yyval.node),"decl",(yyvsp[(3) - (5)].decl).type);
	      Setattr((yyval.node),"parms",(yyvsp[(3) - (5)].decl).parms);
	      Setattr((yyval.node),"value",(yyvsp[(4) - (5)].dtype).val);
	      Setattr((yyval.node),"throws",(yyvsp[(4) - (5)].dtype).throws);
	      Setattr((yyval.node),"throw",(yyvsp[(4) - (5)].dtype).throwf);
	      if (!(yyvsp[(5) - (5)].node)) {
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
	      } else {
		Node *n = (yyvsp[(5) - (5)].node);
		/* Inherit attributes */
		while (n) {
		  String *type = Copy((yyvsp[(2) - (5)].type));
		  Setattr(n,"type",type);
		  Setattr(n,"storage",(yyvsp[(1) - (5)].id));
		  n = nextSibling(n);
		  Delete(type);
		}
	      }
	      if ((yyvsp[(4) - (5)].dtype).bitfield) {
		Setattr((yyval.node),"bitfield", (yyvsp[(4) - (5)].dtype).bitfield);
	      }

	      /* Look for "::" declarations (ignored) */
	      if (Strstr((yyvsp[(3) - (5)].decl).id,"::")) {
                /* This is a special case. If the scope name of the declaration exactly
                   matches that of the declaration, then we will allow it. Otherwise, delete. */
                String *p = Swig_scopename_prefix((yyvsp[(3) - (5)].decl).id);
		if (p) {
		  if ((Namespaceprefix && Strcmp(p,Namespaceprefix) == 0) ||
		      (inclass && Strcmp(p,Classprefix) == 0)) {
		    String *lstr = Swig_scopename_last((yyvsp[(3) - (5)].decl).id);
		    Setattr((yyval.node),"name",lstr);
		    Delete(lstr);
		    set_nextSibling((yyval.node),(yyvsp[(5) - (5)].node));
		  } else {
		    Delete((yyval.node));
		    (yyval.node) = (yyvsp[(5) - (5)].node);
		  }
		  Delete(p);
		} else {
		  Delete((yyval.node));
		  (yyval.node) = (yyvsp[(5) - (5)].node);
		}
	      } else {
		set_nextSibling((yyval.node),(yyvsp[(5) - (5)].node));
	      }
           }
    break;

  case 119:
#line 3159 "parser.y"
    { 
                   (yyval.node) = 0;
                   Clear(scanner_ccode); 
               }
    break;

  case 120:
#line 3163 "parser.y"
    {
		 (yyval.node) = new_node("cdecl");
		 if ((yyvsp[(3) - (4)].dtype).qualifier) SwigType_push((yyvsp[(2) - (4)].decl).type,(yyvsp[(3) - (4)].dtype).qualifier);
		 Setattr((yyval.node),"name",(yyvsp[(2) - (4)].decl).id);
		 Setattr((yyval.node),"decl",(yyvsp[(2) - (4)].decl).type);
		 Setattr((yyval.node),"parms",(yyvsp[(2) - (4)].decl).parms);
		 Setattr((yyval.node),"value",(yyvsp[(3) - (4)].dtype).val);
		 Setattr((yyval.node),"throws",(yyvsp[(3) - (4)].dtype).throws);
		 Setattr((yyval.node),"throw",(yyvsp[(3) - (4)].dtype).throwf);
		 if ((yyvsp[(3) - (4)].dtype).bitfield) {
		   Setattr((yyval.node),"bitfield", (yyvsp[(3) - (4)].dtype).bitfield);
		 }
		 if (!(yyvsp[(4) - (4)].node)) {
		   if (Len(scanner_ccode)) {
		     String *code = Copy(scanner_ccode);
		     Setattr((yyval.node),"code",code);
		     Delete(code);
		   }
		 } else {
		   set_nextSibling((yyval.node),(yyvsp[(4) - (4)].node));
		 }
	       }
    break;

  case 121:
#line 3185 "parser.y"
    { 
                   skip_balanced('{','}');
                   (yyval.node) = 0;
               }
    break;

  case 122:
#line 3191 "parser.y"
    { 
                   (yyval.dtype) = (yyvsp[(1) - (1)].dtype); 
                   (yyval.dtype).qualifier = 0;
		   (yyval.dtype).throws = 0;
		   (yyval.dtype).throwf = 0;
              }
    break;

  case 123:
#line 3197 "parser.y"
    { 
                   (yyval.dtype) = (yyvsp[(2) - (2)].dtype); 
		   (yyval.dtype).qualifier = (yyvsp[(1) - (2)].str);
		   (yyval.dtype).throws = 0;
		   (yyval.dtype).throwf = 0;
	      }
    break;

  case 124:
#line 3203 "parser.y"
    { 
		   (yyval.dtype) = (yyvsp[(5) - (5)].dtype); 
                   (yyval.dtype).qualifier = 0;
		   (yyval.dtype).throws = (yyvsp[(3) - (5)].pl);
		   (yyval.dtype).throwf = NewString("1");
              }
    break;

  case 125:
#line 3209 "parser.y"
    { 
                   (yyval.dtype) = (yyvsp[(6) - (6)].dtype); 
                   (yyval.dtype).qualifier = (yyvsp[(1) - (6)].str);
		   (yyval.dtype).throws = (yyvsp[(4) - (6)].pl);
		   (yyval.dtype).throwf = NewString("1");
              }
    break;

  case 126:
#line 3222 "parser.y"
    {
		   SwigType *ty = 0;
		   (yyval.node) = new_node("enumforward");
		   ty = NewStringf("enum %s", (yyvsp[(3) - (4)].id));
		   Setattr((yyval.node),"name",(yyvsp[(3) - (4)].id));
		   Setattr((yyval.node),"type",ty);
		   Setattr((yyval.node),"sym:weak", "1");
		   add_symbols((yyval.node));
	      }
    break;

  case 127:
#line 3237 "parser.y"
    {
		  SwigType *ty = 0;
                  (yyval.node) = new_node("enum");
		  ty = NewStringf("enum %s", (yyvsp[(3) - (7)].id));
		  Setattr((yyval.node),"name",(yyvsp[(3) - (7)].id));
		  Setattr((yyval.node),"type",ty);
		  appendChild((yyval.node),(yyvsp[(5) - (7)].node));
		  add_symbols((yyval.node));       /* Add to tag space */
		  add_symbols((yyvsp[(5) - (7)].node));       /* Add enum values to id space */
               }
    break;

  case 128:
#line 3247 "parser.y"
    {
		 Node *n;
		 SwigType *ty = 0;
		 String   *unnamed = 0;
		 int       unnamedinstance = 0;

		 (yyval.node) = new_node("enum");
		 if ((yyvsp[(3) - (9)].id)) {
		   Setattr((yyval.node),"name",(yyvsp[(3) - (9)].id));
		   ty = NewStringf("enum %s", (yyvsp[(3) - (9)].id));
		 } else if ((yyvsp[(7) - (9)].decl).id) {
		   unnamed = make_unnamed();
		   ty = NewStringf("enum %s", unnamed);
		   Setattr((yyval.node),"unnamed",unnamed);
                   /* name is not set for unnamed enum instances, e.g. enum { foo } Instance; */
		   if ((yyvsp[(1) - (9)].id) && Cmp((yyvsp[(1) - (9)].id),"typedef") == 0) {
		     Setattr((yyval.node),"name",(yyvsp[(7) - (9)].decl).id);
                   } else {
                     unnamedinstance = 1;
                   }
		   Setattr((yyval.node),"storage",(yyvsp[(1) - (9)].id));
		 }
		 if ((yyvsp[(7) - (9)].decl).id && Cmp((yyvsp[(1) - (9)].id),"typedef") == 0) {
		   Setattr((yyval.node),"tdname",(yyvsp[(7) - (9)].decl).id);
                   Setattr((yyval.node),"allows_typedef","1");
                 }
		 appendChild((yyval.node),(yyvsp[(5) - (9)].node));
		 n = new_node("cdecl");
		 Setattr(n,"type",ty);
		 Setattr(n,"name",(yyvsp[(7) - (9)].decl).id);
		 Setattr(n,"storage",(yyvsp[(1) - (9)].id));
		 Setattr(n,"decl",(yyvsp[(7) - (9)].decl).type);
		 Setattr(n,"parms",(yyvsp[(7) - (9)].decl).parms);
		 Setattr(n,"unnamed",unnamed);

                 if (unnamedinstance) {
		   SwigType *cty = NewString("enum ");
		   Setattr((yyval.node),"type",cty);
		   SetFlag((yyval.node),"unnamedinstance");
		   SetFlag(n,"unnamedinstance");
		   Delete(cty);
                 }
		 if ((yyvsp[(9) - (9)].node)) {
		   Node *p = (yyvsp[(9) - (9)].node);
		   set_nextSibling(n,p);
		   while (p) {
		     SwigType *cty = Copy(ty);
		     Setattr(p,"type",cty);
		     Setattr(p,"unnamed",unnamed);
		     Setattr(p,"storage",(yyvsp[(1) - (9)].id));
		     Delete(cty);
		     p = nextSibling(p);
		   }
		 } else {
		   if (Len(scanner_ccode)) {
		     String *code = Copy(scanner_ccode);
		     Setattr(n,"code",code);
		     Delete(code);
		   }
		 }

                 /* Ensure that typedef enum ABC {foo} XYZ; uses XYZ for sym:name, like structs.
                  * Note that class_rename/yyrename are bit of a mess so used this simple approach to change the name. */
                 if ((yyvsp[(7) - (9)].decl).id && (yyvsp[(3) - (9)].id) && Cmp((yyvsp[(1) - (9)].id),"typedef") == 0) {
		   String *name = NewString((yyvsp[(7) - (9)].decl).id);
                   Setattr((yyval.node), "parser:makename", name);
		   Delete(name);
                 }

		 add_symbols((yyval.node));       /* Add enum to tag space */
		 set_nextSibling((yyval.node),n);
		 Delete(n);
		 add_symbols((yyvsp[(5) - (9)].node));       /* Add enum values to id space */
	         add_symbols(n);
		 Delete(unnamed);
	       }
    break;

  case 129:
#line 3325 "parser.y"
    {
                   /* This is a sick hack.  If the ctor_end has parameters,
                      and the parms parameter only has 1 parameter, this
                      could be a declaration of the form:

                         type (id)(parms)

			 Otherwise it's an error. */
                    int err = 0;
                    (yyval.node) = 0;

		    if ((ParmList_len((yyvsp[(4) - (6)].pl)) == 1) && (!Swig_scopename_check((yyvsp[(2) - (6)].type)))) {
		      SwigType *ty = Getattr((yyvsp[(4) - (6)].pl),"type");
		      String *name = Getattr((yyvsp[(4) - (6)].pl),"name");
		      err = 1;
		      if (!name) {
			(yyval.node) = new_node("cdecl");
			Setattr((yyval.node),"type",(yyvsp[(2) - (6)].type));
			Setattr((yyval.node),"storage",(yyvsp[(1) - (6)].id));
			Setattr((yyval.node),"name",ty);

			if ((yyvsp[(6) - (6)].decl).have_parms) {
			  SwigType *decl = NewStringEmpty();
			  SwigType_add_function(decl,(yyvsp[(6) - (6)].decl).parms);
			  Setattr((yyval.node),"decl",decl);
			  Setattr((yyval.node),"parms",(yyvsp[(6) - (6)].decl).parms);
			  if (Len(scanner_ccode)) {
			    String *code = Copy(scanner_ccode);
			    Setattr((yyval.node),"code",code);
			    Delete(code);
			  }
			}
			if ((yyvsp[(6) - (6)].decl).defarg) {
			  Setattr((yyval.node),"value",(yyvsp[(6) - (6)].decl).defarg);
			}
			Setattr((yyval.node),"throws",(yyvsp[(6) - (6)].decl).throws);
			Setattr((yyval.node),"throw",(yyvsp[(6) - (6)].decl).throwf);
			err = 0;
		      }
		    }
		    if (err) {
		      Swig_error(cparse_file,cparse_line,"Syntax error in input(2).\n");
		      exit(1);
		    }
                }
    break;

  case 130:
#line 3376 "parser.y"
    {  (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 131:
#line 3377 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 132:
#line 3378 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 133:
#line 3379 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 134:
#line 3380 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 135:
#line 3381 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 136:
#line 3386 "parser.y"
    {
                 if (nested_template == 0) {
                   String *prefix;
                   List *bases = 0;
		   Node *scope = 0;
		   (yyval.node) = new_node("class");
		   Setline((yyval.node),cparse_start_line);
		   Setattr((yyval.node),"kind",(yyvsp[(2) - (5)].id));
		   if ((yyvsp[(4) - (5)].bases)) {
		     Setattr((yyval.node),"baselist", Getattr((yyvsp[(4) - (5)].bases),"public"));
		     Setattr((yyval.node),"protectedbaselist", Getattr((yyvsp[(4) - (5)].bases),"protected"));
		     Setattr((yyval.node),"privatebaselist", Getattr((yyvsp[(4) - (5)].bases),"private"));
		   }
		   Setattr((yyval.node),"allows_typedef","1");

		   /* preserve the current scope */
		   prev_symtab = Swig_symbol_current();
		  
		   /* If the class name is qualified.  We need to create or lookup namespace/scope entries */
		   scope = resolve_node_scope((yyvsp[(3) - (5)].str));
		   Setfile(scope,cparse_file);
		   Setline(scope,cparse_line);
		   (yyvsp[(3) - (5)].str) = scope;
		   
		   /* support for old nested classes "pseudo" support, such as:

		         %rename(Ala__Ola) Ala::Ola;
			class Ala::Ola {
			public:
			    Ola() {}
		         };

		      this should disappear when a proper implementation is added.
		   */
		   if (nscope_inner && Strcmp(nodeType(nscope_inner),"namespace") != 0) {
		     if (Namespaceprefix) {
		       String *name = NewStringf("%s::%s", Namespaceprefix, (yyvsp[(3) - (5)].str));		       
		       (yyvsp[(3) - (5)].str) = name;
		       Namespaceprefix = 0;
		       nscope_inner = 0;
		     }
		   }
		   Setattr((yyval.node),"name",(yyvsp[(3) - (5)].str));

		   Delete(class_rename);
                   class_rename = make_name((yyval.node),(yyvsp[(3) - (5)].str),0);
		   Classprefix = NewString((yyvsp[(3) - (5)].str));
		   /* Deal with inheritance  */
		   if ((yyvsp[(4) - (5)].bases)) {
		     bases = make_inherit_list((yyvsp[(3) - (5)].str),Getattr((yyvsp[(4) - (5)].bases),"public"));
		   }
		   prefix = SwigType_istemplate_templateprefix((yyvsp[(3) - (5)].str));
		   if (prefix) {
		     String *fbase, *tbase;
		     if (Namespaceprefix) {
		       fbase = NewStringf("%s::%s", Namespaceprefix,(yyvsp[(3) - (5)].str));
		       tbase = NewStringf("%s::%s", Namespaceprefix, prefix);
		     } else {
		       fbase = Copy((yyvsp[(3) - (5)].str));
		       tbase = Copy(prefix);
		     }
		     Swig_name_inherit(tbase,fbase);
		     Delete(fbase);
		     Delete(tbase);
		   }
                   if (strcmp((yyvsp[(2) - (5)].id),"class") == 0) {
		     cplus_mode = CPLUS_PRIVATE;
		   } else {
		     cplus_mode = CPLUS_PUBLIC;
		   }
		   Swig_symbol_newscope();
		   Swig_symbol_setscopename((yyvsp[(3) - (5)].str));
		   if (bases) {
		     Iterator s;
		     for (s = First(bases); s.item; s = Next(s)) {
		       Symtab *st = Getattr(s.item,"symtab");
		       if (st) {
			 Setfile(st,Getfile(s.item));
			 Setline(st,Getline(s.item));
			 Swig_symbol_inherit(st); 
		       }
		     }
		     Delete(bases);
		   }
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   cparse_start_line = cparse_line;

		   /* If there are active template parameters, we need to make sure they are
                      placed in the class symbol table so we can catch shadows */

		   if (template_parameters) {
		     Parm *tp = template_parameters;
		     while(tp) {
		       String *tpname = Copy(Getattr(tp,"name"));
		       Node *tn = new_node("templateparm");
		       Setattr(tn,"name",tpname);
		       Swig_symbol_cadd(tpname,tn);
		       tp = nextSibling(tp);
		       Delete(tpname);
		     }
		   }
		   if (class_level >= max_class_levels) {
		       if (!max_class_levels) {
			   max_class_levels = 16;
		       } else {
			   max_class_levels *= 2;
		       }
		       class_decl = (Node**) realloc(class_decl, sizeof(Node*) * max_class_levels);
		       if (!class_decl) {
			   Swig_error(cparse_file, cparse_line, "realloc() failed\n");
		       }
		   }
		   class_decl[class_level++] = (yyval.node);
		   Delete(prefix);
		   inclass = 1;
		 }
               }
    break;

  case 137:
#line 3503 "parser.y"
    {
	         (void) (yyvsp[(6) - (9)].node);
		 if (nested_template == 0) {
		   Node *p;
		   SwigType *ty;
		   Symtab *cscope = prev_symtab;
		   Node *am = 0;
		   String *scpname = 0;
		   (yyval.node) = class_decl[--class_level];
		   inclass = 0;
		   
		   /* Check for pure-abstract class */
		   Setattr((yyval.node),"abstract", pure_abstract((yyvsp[(7) - (9)].node)));
		   
		   /* This bit of code merges in a previously defined %extend directive (if any) */
		   
		   if (extendhash) {
		     String *clsname = Swig_symbol_qualifiedscopename(0);
		     am = Getattr(extendhash,clsname);
		     if (am) {
		       merge_extensions((yyval.node),am);
		       Delattr(extendhash,clsname);
		     }
		     Delete(clsname);
		   }
		   if (!classes) classes = NewHash();
		   scpname = Swig_symbol_qualifiedscopename(0);
		   Setattr(classes,scpname,(yyval.node));
		   Delete(scpname);

		   appendChild((yyval.node),(yyvsp[(7) - (9)].node));
		   
		   if (am) append_previous_extension((yyval.node),am);

		   p = (yyvsp[(9) - (9)].node);
		   if (p) {
		     set_nextSibling((yyval.node),p);
		   }
		   
		   if (cparse_cplusplus && !cparse_externc) {
		     ty = NewString((yyvsp[(3) - (9)].str));
		   } else {
		     ty = NewStringf("%s %s", (yyvsp[(2) - (9)].id),(yyvsp[(3) - (9)].str));
		   }
		   while (p) {
		     Setattr(p,"storage",(yyvsp[(1) - (9)].id));
		     Setattr(p,"type",ty);
		     p = nextSibling(p);
		   }
		   /* Dump nested classes */
		   {
		     String *name = (yyvsp[(3) - (9)].str);
		     if ((yyvsp[(9) - (9)].node)) {
		       SwigType *decltype = Getattr((yyvsp[(9) - (9)].node),"decl");
		       if (Cmp((yyvsp[(1) - (9)].id),"typedef") == 0) {
			 if (!decltype || !Len(decltype)) {
			   String *cname;
			   name = Getattr((yyvsp[(9) - (9)].node),"name");
			   cname = Copy(name);
			   Setattr((yyval.node),"tdname",cname);
			   Delete(cname);

			   /* Use typedef name as class name */
			   if (class_rename && (Strcmp(class_rename,(yyvsp[(3) - (9)].str)) == 0)) {
			     Delete(class_rename);
			     class_rename = NewString(name);
			   }
			   if (!Getattr(classes,name)) {
			     Setattr(classes,name,(yyval.node));
			   }
			   Setattr((yyval.node),"decl",decltype);
			 }
		       }
		     }
		     appendChild((yyval.node),dump_nested(Char(name)));
		   }

		   if (cplus_mode != CPLUS_PUBLIC) {
		   /* we 'open' the class at the end, to allow %template
		      to add new members */
		     Node *pa = new_node("access");
		     Setattr(pa,"kind","public");
		     cplus_mode = CPLUS_PUBLIC;
		     appendChild((yyval.node),pa);
		     Delete(pa);
		   }

		   Setattr((yyval.node),"symtab",Swig_symbol_popscope());

		   Classprefix = 0;
		   if (nscope_inner) {
		     /* this is tricky */
		     /* we add the declaration in the original namespace */
		     appendChild(nscope_inner,(yyval.node));
		     Swig_symbol_setscope(Getattr(nscope_inner,"symtab"));
		     Delete(Namespaceprefix);
		     Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		     add_symbols((yyval.node));
		     if (nscope) (yyval.node) = nscope;
		     /* but the variable definition in the current scope */
		     Swig_symbol_setscope(cscope);
		     Delete(Namespaceprefix);
		     Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		     add_symbols((yyvsp[(9) - (9)].node));
		   } else {
		     Delete(yyrename);
		     yyrename = Copy(class_rename);
		     Delete(Namespaceprefix);
		     Namespaceprefix = Swig_symbol_qualifiedscopename(0);

		     add_symbols((yyval.node));
		     add_symbols((yyvsp[(9) - (9)].node));
		   }
		   Swig_symbol_setscope(cscope);
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 } else {
		    (yyval.node) = new_node("class");
		    Setattr((yyval.node),"kind",(yyvsp[(2) - (9)].id));
		    Setattr((yyval.node),"name",NewString((yyvsp[(3) - (9)].str)));
		    SetFlag((yyval.node),"nestedtemplateclass");
		 }
	       }
    break;

  case 138:
#line 3629 "parser.y"
    {
	       String *unnamed;
	       unnamed = make_unnamed();
	       (yyval.node) = new_node("class");
	       Setline((yyval.node),cparse_start_line);
	       Setattr((yyval.node),"kind",(yyvsp[(2) - (3)].id));
	       Setattr((yyval.node),"storage",(yyvsp[(1) - (3)].id));
	       Setattr((yyval.node),"unnamed",unnamed);
	       Setattr((yyval.node),"allows_typedef","1");
	       Delete(class_rename);
	       class_rename = make_name((yyval.node),0,0);
	       if (strcmp((yyvsp[(2) - (3)].id),"class") == 0) {
		 cplus_mode = CPLUS_PRIVATE;
	       } else {
		 cplus_mode = CPLUS_PUBLIC;
	       }
	       Swig_symbol_newscope();
	       cparse_start_line = cparse_line;
	       if (class_level >= max_class_levels) {
		   if (!max_class_levels) {
		       max_class_levels = 16;
		   } else {
		       max_class_levels *= 2;
		   }
		   class_decl = (Node**) realloc(class_decl, sizeof(Node*) * max_class_levels);
		   if (!class_decl) {
		       Swig_error(cparse_file, cparse_line, "realloc() failed\n");
		   }
	       }
	       class_decl[class_level++] = (yyval.node);
	       inclass = 1;
	       Classprefix = NewStringEmpty();
	       Delete(Namespaceprefix);
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
             }
    break;

  case 139:
#line 3663 "parser.y"
    {
	       String *unnamed;
	       Node *n;
	       (void) (yyvsp[(4) - (9)].node);
	       Classprefix = 0;
	       (yyval.node) = class_decl[--class_level];
	       inclass = 0;
	       unnamed = Getattr((yyval.node),"unnamed");

	       /* Check for pure-abstract class */
	       Setattr((yyval.node),"abstract", pure_abstract((yyvsp[(5) - (9)].node)));

	       n = new_node("cdecl");
	       Setattr(n,"name",(yyvsp[(7) - (9)].decl).id);
	       Setattr(n,"unnamed",unnamed);
	       Setattr(n,"type",unnamed);
	       Setattr(n,"decl",(yyvsp[(7) - (9)].decl).type);
	       Setattr(n,"parms",(yyvsp[(7) - (9)].decl).parms);
	       Setattr(n,"storage",(yyvsp[(1) - (9)].id));
	       if ((yyvsp[(9) - (9)].node)) {
		 Node *p = (yyvsp[(9) - (9)].node);
		 set_nextSibling(n,p);
		 while (p) {
		   String *type = Copy(unnamed);
		   Setattr(p,"name",(yyvsp[(7) - (9)].decl).id);
		   Setattr(p,"unnamed",unnamed);
		   Setattr(p,"type",type);
		   Delete(type);
		   Setattr(p,"storage",(yyvsp[(1) - (9)].id));
		   p = nextSibling(p);
		 }
	       }
	       set_nextSibling((yyval.node),n);
	       Delete(n);
	       {
		 /* If a proper typedef name was given, we'll use it to set the scope name */
		 String *name = 0;
		 if ((yyvsp[(1) - (9)].id) && (strcmp((yyvsp[(1) - (9)].id),"typedef") == 0)) {
		   if (!Len((yyvsp[(7) - (9)].decl).type)) {	
		     String *scpname = 0;
		     name = (yyvsp[(7) - (9)].decl).id;
		     Setattr((yyval.node),"tdname",name);
		     Setattr((yyval.node),"name",name);
		     Swig_symbol_setscopename(name);

		     /* If a proper name was given, we use that as the typedef, not unnamed */
		     Clear(unnamed);
		     Append(unnamed, name);
		     
		     n = nextSibling(n);
		     set_nextSibling((yyval.node),n);

		     /* Check for previous extensions */
		     if (extendhash) {
		       String *clsname = Swig_symbol_qualifiedscopename(0);
		       Node *am = Getattr(extendhash,clsname);
		       if (am) {
			 /* Merge the extension into the symbol table */
			 merge_extensions((yyval.node),am);
			 append_previous_extension((yyval.node),am);
			 Delattr(extendhash,clsname);
		       }
		       Delete(clsname);
		     }
		     if (!classes) classes = NewHash();
		     scpname = Swig_symbol_qualifiedscopename(0);
		     Setattr(classes,scpname,(yyval.node));
		     Delete(scpname);
		   } else {
		     Swig_symbol_setscopename("<unnamed>");
		   }
		 }
		 appendChild((yyval.node),(yyvsp[(5) - (9)].node));
		 appendChild((yyval.node),dump_nested(Char(name)));
	       }
	       /* Pop the scope */
	       Setattr((yyval.node),"symtab",Swig_symbol_popscope());
	       if (class_rename) {
		 Delete(yyrename);
		 yyrename = NewString(class_rename);
	       }
	       Delete(Namespaceprefix);
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       add_symbols((yyval.node));
	       add_symbols(n);
	       Delete(unnamed);
              }
    break;

  case 140:
#line 3752 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 141:
#line 3753 "parser.y"
    {
                        (yyval.node) = new_node("cdecl");
                        Setattr((yyval.node),"name",(yyvsp[(1) - (3)].decl).id);
                        Setattr((yyval.node),"decl",(yyvsp[(1) - (3)].decl).type);
                        Setattr((yyval.node),"parms",(yyvsp[(1) - (3)].decl).parms);
			set_nextSibling((yyval.node),(yyvsp[(3) - (3)].node));
                    }
    break;

  case 142:
#line 3765 "parser.y"
    {
              if ((yyvsp[(1) - (4)].id) && (Strcmp((yyvsp[(1) - (4)].id),"friend") == 0)) {
		/* Ignore */
                (yyval.node) = 0; 
	      } else {
		(yyval.node) = new_node("classforward");
		Setfile((yyval.node),cparse_file);
		Setline((yyval.node),cparse_line);
		Setattr((yyval.node),"kind",(yyvsp[(2) - (4)].id));
		Setattr((yyval.node),"name",(yyvsp[(3) - (4)].str));
		Setattr((yyval.node),"sym:weak", "1");
		add_symbols((yyval.node));
	      }
             }
    break;

  case 143:
#line 3785 "parser.y"
    { 
		    template_parameters = (yyvsp[(3) - (4)].tparms); 
		    if (inclass)
		      nested_template++;

		  }
    break;

  case 144:
#line 3790 "parser.y"
    {

		    /* Don't ignore templated functions declared within a class, unless the templated function is within a nested class */
		    if (nested_template <= 1) {
		      int is_nested_template_class = (yyvsp[(6) - (6)].node) && GetFlag((yyvsp[(6) - (6)].node), "nestedtemplateclass");
		      if (is_nested_template_class) {
			(yyval.node) = 0;
			/* Nested template classes would probably better be ignored like ordinary nested classes using cpp_nested, but that introduces shift/reduce conflicts */
			if (cplus_mode == CPLUS_PUBLIC) {
			  /* Treat the nested class/struct/union as a forward declaration until a proper nested class solution is implemented */
			  String *kind = Getattr((yyvsp[(6) - (6)].node), "kind");
			  String *name = Getattr((yyvsp[(6) - (6)].node), "name");
			  (yyval.node) = new_node("template");
			  Setattr((yyval.node),"kind",kind);
			  Setattr((yyval.node),"name",name);
			  Setattr((yyval.node),"sym:weak", "1");
			  Setattr((yyval.node),"templatetype","classforward");
			  Setattr((yyval.node),"templateparms", (yyvsp[(3) - (6)].tparms));
			  add_symbols((yyval.node));

			  if (GetFlag((yyval.node), "feature:nestedworkaround")) {
			    Swig_symbol_remove((yyval.node));
			    (yyval.node) = 0;
			  } else {
			    SWIG_WARN_NODE_BEGIN((yyval.node));
			    Swig_warning(WARN_PARSE_NAMED_NESTED_CLASS, cparse_file, cparse_line, "Nested template %s not currently supported (%s ignored).\n", kind, name);
			    SWIG_WARN_NODE_END((yyval.node));
			  }
			}
			Delete((yyvsp[(6) - (6)].node));
		      } else {
			String *tname = 0;
			int     error = 0;

			/* check if we get a namespace node with a class declaration, and retrieve the class */
			Symtab *cscope = Swig_symbol_current();
			Symtab *sti = 0;
			Node *ntop = (yyvsp[(6) - (6)].node);
			Node *ni = ntop;
			SwigType *ntype = ni ? nodeType(ni) : 0;
			while (ni && Strcmp(ntype,"namespace") == 0) {
			  sti = Getattr(ni,"symtab");
			  ni = firstChild(ni);
			  ntype = nodeType(ni);
			}
			if (sti) {
			  Swig_symbol_setscope(sti);
			  Delete(Namespaceprefix);
			  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
			  (yyvsp[(6) - (6)].node) = ni;
			}

			(yyval.node) = (yyvsp[(6) - (6)].node);
			if ((yyval.node)) tname = Getattr((yyval.node),"name");
			
			/* Check if the class is a template specialization */
			if (((yyval.node)) && (Strchr(tname,'<')) && (!is_operator(tname))) {
			  /* If a specialization.  Check if defined. */
			  Node *tempn = 0;
			  {
			    String *tbase = SwigType_templateprefix(tname);
			    tempn = Swig_symbol_clookup_local(tbase,0);
			    if (!tempn || (Strcmp(nodeType(tempn),"template") != 0)) {
			      SWIG_WARN_NODE_BEGIN(tempn);
			      Swig_warning(WARN_PARSE_TEMPLATE_SP_UNDEF, Getfile((yyval.node)),Getline((yyval.node)),"Specialization of non-template '%s'.\n", tbase);
			      SWIG_WARN_NODE_END(tempn);
			      tempn = 0;
			      error = 1;
			    }
			    Delete(tbase);
			  }
			  Setattr((yyval.node),"specialization","1");
			  Setattr((yyval.node),"templatetype",nodeType((yyval.node)));
			  set_nodeType((yyval.node),"template");
			  /* Template partial specialization */
			  if (tempn && ((yyvsp[(3) - (6)].tparms)) && ((yyvsp[(6) - (6)].node))) {
			    List   *tlist;
			    String *targs = SwigType_templateargs(tname);
			    tlist = SwigType_parmlist(targs);
			    /*			  Printf(stdout,"targs = '%s' %s\n", targs, tlist); */
			    if (!Getattr((yyval.node),"sym:weak")) {
			      Setattr((yyval.node),"sym:typename","1");
			    }
			    
			    if (Len(tlist) != ParmList_len(Getattr(tempn,"templateparms"))) {
			      Swig_error(Getfile((yyval.node)),Getline((yyval.node)),"Inconsistent argument count in template partial specialization. %d %d\n", Len(tlist), ParmList_len(Getattr(tempn,"templateparms")));
			      
			    } else {

			    /* This code builds the argument list for the partial template
			       specialization.  This is a little hairy, but the idea is as
			       follows:

			       $3 contains a list of arguments supplied for the template.
			       For example template<class T>.

			       tlist is a list of the specialization arguments--which may be
			       different.  For example class<int,T>.

			       tp is a copy of the arguments in the original template definition.
       
			       The patching algorithm walks through the list of supplied
			       arguments ($3), finds the position in the specialization arguments
			       (tlist), and then patches the name in the argument list of the
			       original template.
			    */

			    {
			      String *pn;
			      Parm *p, *p1;
			      int i, nargs;
			      Parm *tp = CopyParmList(Getattr(tempn,"templateparms"));
			      nargs = Len(tlist);
			      p = (yyvsp[(3) - (6)].tparms);
			      while (p) {
				for (i = 0; i < nargs; i++){
				  pn = Getattr(p,"name");
				  if (Strcmp(pn,SwigType_base(Getitem(tlist,i))) == 0) {
				    int j;
				    Parm *p1 = tp;
				    for (j = 0; j < i; j++) {
				      p1 = nextSibling(p1);
				    }
				    Setattr(p1,"name",pn);
				    Setattr(p1,"partialarg","1");
				  }
				}
				p = nextSibling(p);
			      }
			      p1 = tp;
			      i = 0;
			      while (p1) {
				if (!Getattr(p1,"partialarg")) {
				  Delattr(p1,"name");
				  Setattr(p1,"type", Getitem(tlist,i));
				} 
				i++;
				p1 = nextSibling(p1);
			      }
			      Setattr((yyval.node),"templateparms",tp);
			      Delete(tp);
			    }
  #if 0
			    /* Patch the parameter list */
			    if (tempn) {
			      Parm *p,*p1;
			      ParmList *tp = CopyParmList(Getattr(tempn,"templateparms"));
			      p = (yyvsp[(3) - (6)].tparms);
			      p1 = tp;
			      while (p && p1) {
				String *pn = Getattr(p,"name");
				Printf(stdout,"pn = '%s'\n", pn);
				if (pn) Setattr(p1,"name",pn);
				else Delattr(p1,"name");
				pn = Getattr(p,"type");
				if (pn) Setattr(p1,"type",pn);
				p = nextSibling(p);
				p1 = nextSibling(p1);
			      }
			      Setattr((yyval.node),"templateparms",tp);
			      Delete(tp);
			    } else {
			      Setattr((yyval.node),"templateparms",(yyvsp[(3) - (6)].tparms));
			    }
  #endif
			    Delattr((yyval.node),"specialization");
			    Setattr((yyval.node),"partialspecialization","1");
			    /* Create a specialized name for matching */
			    {
			      Parm *p = (yyvsp[(3) - (6)].tparms);
			      String *fname = NewString(Getattr((yyval.node),"name"));
			      String *ffname = 0;
			      ParmList *partialparms = 0;

			      char   tmp[32];
			      int    i, ilen;
			      while (p) {
				String *n = Getattr(p,"name");
				if (!n) {
				  p = nextSibling(p);
				  continue;
				}
				ilen = Len(tlist);
				for (i = 0; i < ilen; i++) {
				  if (Strstr(Getitem(tlist,i),n)) {
				    sprintf(tmp,"$%d",i+1);
				    Replaceid(fname,n,tmp);
				  }
				}
				p = nextSibling(p);
			      }
			      /* Patch argument names with typedef */
			      {
				Iterator tt;
				Parm *parm_current = 0;
				List *tparms = SwigType_parmlist(fname);
				ffname = SwigType_templateprefix(fname);
				Append(ffname,"<(");
				for (tt = First(tparms); tt.item; ) {
				  SwigType *rtt = Swig_symbol_typedef_reduce(tt.item,0);
				  SwigType *ttr = Swig_symbol_type_qualify(rtt,0);

				  Parm *newp = NewParmWithoutFileLineInfo(ttr, 0);
				  if (partialparms)
				    set_nextSibling(parm_current, newp);
				  else
				    partialparms = newp;
				  parm_current = newp;

				  Append(ffname,ttr);
				  tt = Next(tt);
				  if (tt.item) Putc(',',ffname);
				  Delete(rtt);
				  Delete(ttr);
				}
				Delete(tparms);
				Append(ffname,")>");
			      }
			      {
				Node *new_partial = NewHash();
				String *partials = Getattr(tempn,"partials");
				if (!partials) {
				  partials = NewList();
				  Setattr(tempn,"partials",partials);
				  Delete(partials);
				}
				/*			      Printf(stdout,"partial: fname = '%s', '%s'\n", fname, Swig_symbol_typedef_reduce(fname,0)); */
				Setattr(new_partial, "partialparms", partialparms);
				Setattr(new_partial, "templcsymname", ffname);
				Append(partials, new_partial);
			      }
			      Setattr((yyval.node),"partialargs",ffname);
			      Swig_symbol_cadd(ffname,(yyval.node));
			    }
			    }
			    Delete(tlist);
			    Delete(targs);
			  } else {
			    /* An explicit template specialization */
			    /* add default args from primary (unspecialized) template */
			    String *ty = Swig_symbol_template_deftype(tname,0);
			    String *fname = Swig_symbol_type_qualify(ty,0);
			    Swig_symbol_cadd(fname,(yyval.node));
			    Delete(ty);
			    Delete(fname);
			  }
			}  else if ((yyval.node)) {
			  Setattr((yyval.node),"templatetype",nodeType((yyvsp[(6) - (6)].node)));
			  set_nodeType((yyval.node),"template");
			  Setattr((yyval.node),"templateparms", (yyvsp[(3) - (6)].tparms));
			  if (!Getattr((yyval.node),"sym:weak")) {
			    Setattr((yyval.node),"sym:typename","1");
			  }
			  add_symbols((yyval.node));
			  default_arguments((yyval.node));
			  /* We also place a fully parameterized version in the symbol table */
			  {
			    Parm *p;
			    String *fname = NewStringf("%s<(", Getattr((yyval.node),"name"));
			    p = (yyvsp[(3) - (6)].tparms);
			    while (p) {
			      String *n = Getattr(p,"name");
			      if (!n) n = Getattr(p,"type");
			      Append(fname,n);
			      p = nextSibling(p);
			      if (p) Putc(',',fname);
			    }
			    Append(fname,")>");
			    Swig_symbol_cadd(fname,(yyval.node));
			  }
			}
			(yyval.node) = ntop;
			Swig_symbol_setscope(cscope);
			Delete(Namespaceprefix);
			Namespaceprefix = Swig_symbol_qualifiedscopename(0);
			if (error) (yyval.node) = 0;
		      }
		    } else {
		      (yyval.node) = 0;
		    }
		    template_parameters = 0;
		    if (inclass)
		      nested_template--;
                  }
    break;

  case 145:
#line 4074 "parser.y"
    {
		  Swig_warning(WARN_PARSE_EXPLICIT_TEMPLATE, cparse_file, cparse_line, "Explicit template instantiation ignored.\n");
                   (yyval.node) = 0; 
                }
    break;

  case 146:
#line 4080 "parser.y"
    {
		  (yyval.node) = (yyvsp[(1) - (1)].node);
                }
    break;

  case 147:
#line 4083 "parser.y"
    {
                   (yyval.node) = (yyvsp[(1) - (1)].node);
                }
    break;

  case 148:
#line 4086 "parser.y"
    {
                   (yyval.node) = (yyvsp[(1) - (1)].node);
                }
    break;

  case 149:
#line 4089 "parser.y"
    {
		  (yyval.node) = 0;
                }
    break;

  case 150:
#line 4092 "parser.y"
    {
                  (yyval.node) = (yyvsp[(1) - (1)].node);
                }
    break;

  case 151:
#line 4095 "parser.y"
    {
                  (yyval.node) = (yyvsp[(1) - (1)].node);
                }
    break;

  case 152:
#line 4100 "parser.y"
    {
		   /* Rip out the parameter names */
		  Parm *p = (yyvsp[(1) - (1)].pl);
		  (yyval.tparms) = (yyvsp[(1) - (1)].pl);

		  while (p) {
		    String *name = Getattr(p,"name");
		    if (!name) {
		      /* Hmmm. Maybe it's a 'class T' parameter */
		      char *type = Char(Getattr(p,"type"));
		      /* Template template parameter */
		      if (strncmp(type,"template<class> ",16) == 0) {
			type += 16;
		      }
		      if ((strncmp(type,"class ",6) == 0) || (strncmp(type,"typename ", 9) == 0)) {
			char *t = strchr(type,' ');
			Setattr(p,"name", t+1);
		      } else {
			/*
			 Swig_error(cparse_file, cparse_line, "Missing template parameter name\n");
			 $$.rparms = 0;
			 $$.parms = 0;
			 break; */
		      }
		    }
		    p = nextSibling(p);
		  }
                 }
    break;

  case 153:
#line 4130 "parser.y"
    {
                      set_nextSibling((yyvsp[(1) - (2)].p),(yyvsp[(2) - (2)].pl));
                      (yyval.pl) = (yyvsp[(1) - (2)].p);
                   }
    break;

  case 154:
#line 4134 "parser.y"
    { (yyval.pl) = 0; }
    break;

  case 155:
#line 4137 "parser.y"
    {
		    (yyval.p) = NewParmWithoutFileLineInfo(NewString((yyvsp[(1) - (1)].id)), 0);
                  }
    break;

  case 156:
#line 4140 "parser.y"
    {
                    (yyval.p) = (yyvsp[(1) - (1)].p);
                  }
    break;

  case 157:
#line 4145 "parser.y"
    {
                         set_nextSibling((yyvsp[(2) - (3)].p),(yyvsp[(3) - (3)].pl));
                         (yyval.pl) = (yyvsp[(2) - (3)].p);
                       }
    break;

  case 158:
#line 4149 "parser.y"
    { (yyval.pl) = 0; }
    break;

  case 159:
#line 4154 "parser.y"
    {
                  String *uname = Swig_symbol_type_qualify((yyvsp[(2) - (3)].str),0);
		  String *name = Swig_scopename_last((yyvsp[(2) - (3)].str));
                  (yyval.node) = new_node("using");
		  Setattr((yyval.node),"uname",uname);
		  Setattr((yyval.node),"name", name);
		  Delete(uname);
		  Delete(name);
		  add_symbols((yyval.node));
             }
    break;

  case 160:
#line 4164 "parser.y"
    {
	       Node *n = Swig_symbol_clookup((yyvsp[(3) - (4)].str),0);
	       if (!n) {
		 Swig_error(cparse_file, cparse_line, "Nothing known about namespace '%s'\n", (yyvsp[(3) - (4)].str));
		 (yyval.node) = 0;
	       } else {

		 while (Strcmp(nodeType(n),"using") == 0) {
		   n = Getattr(n,"node");
		 }
		 if (n) {
		   if (Strcmp(nodeType(n),"namespace") == 0) {
		     Symtab *current = Swig_symbol_current();
		     Symtab *symtab = Getattr(n,"symtab");
		     (yyval.node) = new_node("using");
		     Setattr((yyval.node),"node",n);
		     Setattr((yyval.node),"namespace", (yyvsp[(3) - (4)].str));
		     if (current != symtab) {
		       Swig_symbol_inherit(symtab);
		     }
		   } else {
		     Swig_error(cparse_file, cparse_line, "'%s' is not a namespace.\n", (yyvsp[(3) - (4)].str));
		     (yyval.node) = 0;
		   }
		 } else {
		   (yyval.node) = 0;
		 }
	       }
             }
    break;

  case 161:
#line 4195 "parser.y"
    { 
                Hash *h;
                (yyvsp[(1) - (3)].node) = Swig_symbol_current();
		h = Swig_symbol_clookup((yyvsp[(2) - (3)].str),0);
		if (h && ((yyvsp[(1) - (3)].node) == Getattr(h,"sym:symtab")) && (Strcmp(nodeType(h),"namespace") == 0)) {
		  if (Getattr(h,"alias")) {
		    h = Getattr(h,"namespace");
		    Swig_warning(WARN_PARSE_NAMESPACE_ALIAS, cparse_file, cparse_line, "Namespace alias '%s' not allowed here. Assuming '%s'\n",
				 (yyvsp[(2) - (3)].str), Getattr(h,"name"));
		    (yyvsp[(2) - (3)].str) = Getattr(h,"name");
		  }
		  Swig_symbol_setscope(Getattr(h,"symtab"));
		} else {
		  Swig_symbol_newscope();
		  Swig_symbol_setscopename((yyvsp[(2) - (3)].str));
		}
		Delete(Namespaceprefix);
		Namespaceprefix = Swig_symbol_qualifiedscopename(0);
             }
    break;

  case 162:
#line 4213 "parser.y"
    {
                Node *n = (yyvsp[(5) - (6)].node);
		set_nodeType(n,"namespace");
		Setattr(n,"name",(yyvsp[(2) - (6)].str));
                Setattr(n,"symtab", Swig_symbol_popscope());
		Swig_symbol_setscope((yyvsp[(1) - (6)].node));
		(yyval.node) = n;
		Delete(Namespaceprefix);
		Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		add_symbols((yyval.node));
             }
    break;

  case 163:
#line 4224 "parser.y"
    {
	       Hash *h;
	       (yyvsp[(1) - (2)].node) = Swig_symbol_current();
	       h = Swig_symbol_clookup((char *)"    ",0);
	       if (h && (Strcmp(nodeType(h),"namespace") == 0)) {
		 Swig_symbol_setscope(Getattr(h,"symtab"));
	       } else {
		 Swig_symbol_newscope();
		 /* we don't use "__unnamed__", but a long 'empty' name */
		 Swig_symbol_setscopename("    ");
	       }
	       Namespaceprefix = 0;
             }
    break;

  case 164:
#line 4236 "parser.y"
    {
	       (yyval.node) = (yyvsp[(4) - (5)].node);
	       set_nodeType((yyval.node),"namespace");
	       Setattr((yyval.node),"unnamed","1");
	       Setattr((yyval.node),"symtab", Swig_symbol_popscope());
	       Swig_symbol_setscope((yyvsp[(1) - (5)].node));
	       Delete(Namespaceprefix);
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       add_symbols((yyval.node));
             }
    break;

  case 165:
#line 4246 "parser.y"
    {
	       /* Namespace alias */
	       Node *n;
	       (yyval.node) = new_node("namespace");
	       Setattr((yyval.node),"name",(yyvsp[(2) - (5)].id));
	       Setattr((yyval.node),"alias",(yyvsp[(4) - (5)].str));
	       n = Swig_symbol_clookup((yyvsp[(4) - (5)].str),0);
	       if (!n) {
		 Swig_error(cparse_file, cparse_line, "Unknown namespace '%s'\n", (yyvsp[(4) - (5)].str));
		 (yyval.node) = 0;
	       } else {
		 if (Strcmp(nodeType(n),"namespace") != 0) {
		   Swig_error(cparse_file, cparse_line, "'%s' is not a namespace\n",(yyvsp[(4) - (5)].str));
		   (yyval.node) = 0;
		 } else {
		   while (Getattr(n,"alias")) {
		     n = Getattr(n,"namespace");
		   }
		   Setattr((yyval.node),"namespace",n);
		   add_symbols((yyval.node));
		   /* Set up a scope alias */
		   Swig_symbol_alias((yyvsp[(2) - (5)].id),Getattr(n,"symtab"));
		 }
	       }
             }
    break;

  case 166:
#line 4273 "parser.y"
    {
                   (yyval.node) = (yyvsp[(1) - (2)].node);
                   /* Insert cpp_member (including any siblings) to the front of the cpp_members linked list */
		   if ((yyval.node)) {
		     Node *p = (yyval.node);
		     Node *pp =0;
		     while (p) {
		       pp = p;
		       p = nextSibling(p);
		     }
		     set_nextSibling(pp,(yyvsp[(2) - (2)].node));
		   } else {
		     (yyval.node) = (yyvsp[(2) - (2)].node);
		   }
             }
    break;

  case 167:
#line 4288 "parser.y"
    { 
                  if (cplus_mode != CPLUS_PUBLIC) {
		     Swig_error(cparse_file,cparse_line,"%%extend can only be used in a public section\n");
		  }
             }
    break;

  case 168:
#line 4292 "parser.y"
    {
	       (yyval.node) = new_node("extend");
	       tag_nodes((yyvsp[(4) - (6)].node),"feature:extend",(char*) "1");
	       appendChild((yyval.node),(yyvsp[(4) - (6)].node));
	       set_nextSibling((yyval.node),(yyvsp[(6) - (6)].node));
	     }
    break;

  case 169:
#line 4298 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 170:
#line 4299 "parser.y"
    { (yyval.node) = 0;}
    break;

  case 171:
#line 4300 "parser.y"
    {
	       int start_line = cparse_line;
	       skip_decl();
	       Swig_error(cparse_file,start_line,"Syntax error in input(3).\n");
	       exit(1);
	       }
    break;

  case 172:
#line 4305 "parser.y"
    { 
		 (yyval.node) = (yyvsp[(3) - (3)].node);
   	     }
    break;

  case 173:
#line 4316 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 174:
#line 4317 "parser.y"
    { 
                 (yyval.node) = (yyvsp[(1) - (1)].node); 
		 if (extendmode) {
		   String *symname;
		   symname= make_name((yyval.node),Getattr((yyval.node),"name"), Getattr((yyval.node),"decl"));
		   if (Strcmp(symname,Getattr((yyval.node),"name")) == 0) {
		     /* No renaming operation.  Set name to class name */
		     Delete(yyrename);
		     yyrename = NewString(Getattr(current_class,"sym:name"));
		   } else {
		     Delete(yyrename);
		     yyrename = symname;
		   }
		 }
		 add_symbols((yyval.node));
                 default_arguments((yyval.node));
             }
    break;

  case 175:
#line 4334 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 176:
#line 4335 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 177:
#line 4336 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 178:
#line 4337 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 179:
#line 4338 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 180:
#line 4339 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 181:
#line 4340 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 182:
#line 4341 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 183:
#line 4342 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 184:
#line 4343 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 185:
#line 4344 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 186:
#line 4345 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 187:
#line 4346 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 188:
#line 4347 "parser.y"
    {(yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 189:
#line 4348 "parser.y"
    {(yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 190:
#line 4349 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 191:
#line 4358 "parser.y"
    {
              if (Classprefix) {
		 SwigType *decl = NewStringEmpty();
		 (yyval.node) = new_node("constructor");
		 Setattr((yyval.node),"storage",(yyvsp[(1) - (6)].id));
		 Setattr((yyval.node),"name",(yyvsp[(2) - (6)].type));
		 Setattr((yyval.node),"parms",(yyvsp[(4) - (6)].pl));
		 SwigType_add_function(decl,(yyvsp[(4) - (6)].pl));
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"throws",(yyvsp[(6) - (6)].decl).throws);
		 Setattr((yyval.node),"throw",(yyvsp[(6) - (6)].decl).throwf);
		 if (Len(scanner_ccode)) {
		   String *code = Copy(scanner_ccode);
		   Setattr((yyval.node),"code",code);
		   Delete(code);
		 }
		 SetFlag((yyval.node),"feature:new");
	      } else {
		(yyval.node) = 0;
              }
              }
    break;

  case 192:
#line 4383 "parser.y"
    {
               String *name = NewStringf("%s",(yyvsp[(2) - (6)].str));
	       if (*(Char(name)) != '~') Insert(name,0,"~");
               (yyval.node) = new_node("destructor");
	       Setattr((yyval.node),"name",name);
	       Delete(name);
	       if (Len(scanner_ccode)) {
		 String *code = Copy(scanner_ccode);
		 Setattr((yyval.node),"code",code);
		 Delete(code);
	       }
	       {
		 String *decl = NewStringEmpty();
		 SwigType_add_function(decl,(yyvsp[(4) - (6)].pl));
		 Setattr((yyval.node),"decl",decl);
		 Delete(decl);
	       }
	       Setattr((yyval.node),"throws",(yyvsp[(6) - (6)].dtype).throws);
	       Setattr((yyval.node),"throw",(yyvsp[(6) - (6)].dtype).throwf);
	       add_symbols((yyval.node));
	      }
    break;

  case 193:
#line 4407 "parser.y"
    {
		String *name;
		char *c = 0;
		(yyval.node) = new_node("destructor");
	       /* Check for template names.  If the class is a template
		  and the constructor is missing the template part, we
		  add it */
	        if (Classprefix) {
                  c = strchr(Char(Classprefix),'<');
                  if (c && !Strchr((yyvsp[(3) - (7)].str),'<')) {
                    (yyvsp[(3) - (7)].str) = NewStringf("%s%s",(yyvsp[(3) - (7)].str),c);
                  }
		}
		Setattr((yyval.node),"storage","virtual");
	        name = NewStringf("%s",(yyvsp[(3) - (7)].str));
		if (*(Char(name)) != '~') Insert(name,0,"~");
		Setattr((yyval.node),"name",name);
		Delete(name);
		Setattr((yyval.node),"throws",(yyvsp[(7) - (7)].dtype).throws);
		Setattr((yyval.node),"throw",(yyvsp[(7) - (7)].dtype).throwf);
		if ((yyvsp[(7) - (7)].dtype).val) {
		  Setattr((yyval.node),"value","0");
		}
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
		{
		  String *decl = NewStringEmpty();
		  SwigType_add_function(decl,(yyvsp[(5) - (7)].pl));
		  Setattr((yyval.node),"decl",decl);
		  Delete(decl);
		}

		add_symbols((yyval.node));
	      }
    break;

  case 194:
#line 4448 "parser.y"
    {
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[(3) - (8)].type));
		 Setattr((yyval.node),"name",(yyvsp[(2) - (8)].str));
		 Setattr((yyval.node),"storage",(yyvsp[(1) - (8)].id));

		 SwigType_add_function((yyvsp[(4) - (8)].type),(yyvsp[(6) - (8)].pl));
		 if ((yyvsp[(8) - (8)].dtype).qualifier) {
		   SwigType_push((yyvsp[(4) - (8)].type),(yyvsp[(8) - (8)].dtype).qualifier);
		 }
		 Setattr((yyval.node),"decl",(yyvsp[(4) - (8)].type));
		 Setattr((yyval.node),"parms",(yyvsp[(6) - (8)].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
              }
    break;

  case 195:
#line 4463 "parser.y"
    {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[(3) - (8)].type));
		 Setattr((yyval.node),"name",(yyvsp[(2) - (8)].str));
		 Setattr((yyval.node),"storage",(yyvsp[(1) - (8)].id));
		 decl = NewStringEmpty();
		 SwigType_add_reference(decl);
		 SwigType_add_function(decl,(yyvsp[(6) - (8)].pl));
		 if ((yyvsp[(8) - (8)].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[(8) - (8)].dtype).qualifier);
		 }
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[(6) - (8)].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
	       }
    break;

  case 196:
#line 4481 "parser.y"
    {
		String *t = NewStringEmpty();
		(yyval.node) = new_node("cdecl");
		Setattr((yyval.node),"type",(yyvsp[(3) - (7)].type));
		Setattr((yyval.node),"name",(yyvsp[(2) - (7)].str));
		 Setattr((yyval.node),"storage",(yyvsp[(1) - (7)].id));
		SwigType_add_function(t,(yyvsp[(5) - (7)].pl));
		if ((yyvsp[(7) - (7)].dtype).qualifier) {
		  SwigType_push(t,(yyvsp[(7) - (7)].dtype).qualifier);
		}
		Setattr((yyval.node),"decl",t);
		Setattr((yyval.node),"parms",(yyvsp[(5) - (7)].pl));
		Setattr((yyval.node),"conversion_operator","1");
		add_symbols((yyval.node));
              }
    break;

  case 197:
#line 4500 "parser.y"
    {
                 skip_balanced('{','}');
                 (yyval.node) = 0;
               }
    break;

  case 198:
#line 4507 "parser.y"
    { 
                (yyval.node) = new_node("access");
		Setattr((yyval.node),"kind","public");
                cplus_mode = CPLUS_PUBLIC;
              }
    break;

  case 199:
#line 4514 "parser.y"
    { 
                (yyval.node) = new_node("access");
                Setattr((yyval.node),"kind","private");
		cplus_mode = CPLUS_PRIVATE;
	      }
    break;

  case 200:
#line 4522 "parser.y"
    { 
		(yyval.node) = new_node("access");
		Setattr((yyval.node),"kind","protected");
		cplus_mode = CPLUS_PROTECTED;
	      }
    break;

  case 201:
#line 4543 "parser.y"
    {
		cparse_start_line = cparse_line; skip_balanced('{','}');
		(yyval.str) = NewString(scanner_ccode); /* copied as initializers overwrite scanner_ccode */
	      }
    break;

  case 202:
#line 4546 "parser.y"
    {
	        (yyval.node) = 0;
		if (cplus_mode == CPLUS_PUBLIC) {
		  if (cparse_cplusplus) {
		    (yyval.node) = nested_forward_declaration((yyvsp[(1) - (7)].id), (yyvsp[(2) - (7)].id), (yyvsp[(3) - (7)].str), (yyvsp[(3) - (7)].str), (yyvsp[(7) - (7)].node));
		  } else if ((yyvsp[(7) - (7)].node)) {
		    nested_new_struct((yyvsp[(2) - (7)].id), (yyvsp[(6) - (7)].str), (yyvsp[(7) - (7)].node));
		  }
		}
		Delete((yyvsp[(6) - (7)].str));
	      }
    break;

  case 203:
#line 4568 "parser.y"
    {
		cparse_start_line = cparse_line; skip_balanced('{','}');
		(yyval.str) = NewString(scanner_ccode); /* copied as initializers overwrite scanner_ccode */
	      }
    break;

  case 204:
#line 4571 "parser.y"
    {
	        (yyval.node) = 0;
		if (cplus_mode == CPLUS_PUBLIC) {
		  if (cparse_cplusplus) {
		    const char *name = (yyvsp[(6) - (6)].node) ? Getattr((yyvsp[(6) - (6)].node), "name") : 0;
		    (yyval.node) = nested_forward_declaration((yyvsp[(1) - (6)].id), (yyvsp[(2) - (6)].id), 0, name, (yyvsp[(6) - (6)].node));
		  } else {
		    if ((yyvsp[(6) - (6)].node)) {
		      nested_new_struct((yyvsp[(2) - (6)].id), (yyvsp[(5) - (6)].str), (yyvsp[(6) - (6)].node));
		    } else {
		      Swig_warning(WARN_PARSE_UNNAMED_NESTED_CLASS, cparse_file, cparse_line, "Nested %s not currently supported (ignored).\n", (yyvsp[(2) - (6)].id));
		    }
		  }
		}
		Delete((yyvsp[(5) - (6)].str));
	      }
    break;

  case 205:
#line 4603 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 206:
#line 4606 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 207:
#line 4610 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 208:
#line 4613 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 209:
#line 4614 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 210:
#line 4615 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 211:
#line 4616 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 212:
#line 4617 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 213:
#line 4618 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 214:
#line 4619 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 215:
#line 4620 "parser.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); }
    break;

  case 216:
#line 4623 "parser.y"
    {
	            Clear(scanner_ccode);
		    (yyval.dtype).throws = (yyvsp[(1) - (2)].dtype).throws;
		    (yyval.dtype).throwf = (yyvsp[(1) - (2)].dtype).throwf;
               }
    break;

  case 217:
#line 4628 "parser.y"
    { 
		    skip_balanced('{','}'); 
		    (yyval.dtype).throws = (yyvsp[(1) - (2)].dtype).throws;
		    (yyval.dtype).throwf = (yyvsp[(1) - (2)].dtype).throwf;
	       }
    break;

  case 218:
#line 4635 "parser.y"
    { 
                     Clear(scanner_ccode);
                     (yyval.dtype).val = 0;
                     (yyval.dtype).qualifier = (yyvsp[(1) - (2)].dtype).qualifier;
                     (yyval.dtype).bitfield = 0;
                     (yyval.dtype).throws = (yyvsp[(1) - (2)].dtype).throws;
                     (yyval.dtype).throwf = (yyvsp[(1) - (2)].dtype).throwf;
                }
    break;

  case 219:
#line 4643 "parser.y"
    { 
                     Clear(scanner_ccode);
                     (yyval.dtype).val = (yyvsp[(3) - (4)].dtype).val;
                     (yyval.dtype).qualifier = (yyvsp[(1) - (4)].dtype).qualifier;
                     (yyval.dtype).bitfield = 0;
                     (yyval.dtype).throws = (yyvsp[(1) - (4)].dtype).throws; 
                     (yyval.dtype).throwf = (yyvsp[(1) - (4)].dtype).throwf; 
               }
    break;

  case 220:
#line 4651 "parser.y"
    { 
                     skip_balanced('{','}');
                     (yyval.dtype).val = 0;
                     (yyval.dtype).qualifier = (yyvsp[(1) - (2)].dtype).qualifier;
                     (yyval.dtype).bitfield = 0;
                     (yyval.dtype).throws = (yyvsp[(1) - (2)].dtype).throws; 
                     (yyval.dtype).throwf = (yyvsp[(1) - (2)].dtype).throwf; 
               }
    break;

  case 221:
#line 4662 "parser.y"
    { }
    break;

  case 222:
#line 4668 "parser.y"
    { (yyval.id) = "extern"; }
    break;

  case 223:
#line 4669 "parser.y"
    { 
                   if (strcmp((yyvsp[(2) - (2)].id),"C") == 0) {
		     (yyval.id) = "externc";
		   } else {
		     Swig_warning(WARN_PARSE_UNDEFINED_EXTERN,cparse_file, cparse_line,"Unrecognized extern type \"%s\".\n", (yyvsp[(2) - (2)].id));
		     (yyval.id) = 0;
		   }
               }
    break;

  case 224:
#line 4677 "parser.y"
    { (yyval.id) = "static"; }
    break;

  case 225:
#line 4678 "parser.y"
    { (yyval.id) = "typedef"; }
    break;

  case 226:
#line 4679 "parser.y"
    { (yyval.id) = "virtual"; }
    break;

  case 227:
#line 4680 "parser.y"
    { (yyval.id) = "friend"; }
    break;

  case 228:
#line 4681 "parser.y"
    { (yyval.id) = "explicit"; }
    break;

  case 229:
#line 4682 "parser.y"
    { (yyval.id) = 0; }
    break;

  case 230:
#line 4689 "parser.y"
    {
                 Parm *p;
		 (yyval.pl) = (yyvsp[(1) - (1)].pl);
		 p = (yyvsp[(1) - (1)].pl);
                 while (p) {
		   Replace(Getattr(p,"type"),"typename ", "", DOH_REPLACE_ANY);
		   p = nextSibling(p);
                 }
               }
    break;

  case 231:
#line 4700 "parser.y"
    {
                  set_nextSibling((yyvsp[(1) - (2)].p),(yyvsp[(2) - (2)].pl));
                  (yyval.pl) = (yyvsp[(1) - (2)].p);
		}
    break;

  case 232:
#line 4704 "parser.y"
    { (yyval.pl) = 0; }
    break;

  case 233:
#line 4707 "parser.y"
    {
                 set_nextSibling((yyvsp[(2) - (3)].p),(yyvsp[(3) - (3)].pl));
		 (yyval.pl) = (yyvsp[(2) - (3)].p);
                }
    break;

  case 234:
#line 4711 "parser.y"
    { (yyval.pl) = 0; }
    break;

  case 235:
#line 4715 "parser.y"
    {
                   SwigType_push((yyvsp[(1) - (2)].type),(yyvsp[(2) - (2)].decl).type);
		   (yyval.p) = NewParmWithoutFileLineInfo((yyvsp[(1) - (2)].type),(yyvsp[(2) - (2)].decl).id);
		   Setfile((yyval.p),cparse_file);
		   Setline((yyval.p),cparse_line);
		   if ((yyvsp[(2) - (2)].decl).defarg) {
		     Setattr((yyval.p),"value",(yyvsp[(2) - (2)].decl).defarg);
		   }
		}
    break;

  case 236:
#line 4725 "parser.y"
    {
                  (yyval.p) = NewParmWithoutFileLineInfo(NewStringf("template<class> %s %s", (yyvsp[(5) - (7)].id),(yyvsp[(6) - (7)].str)), 0);
		  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
                  if ((yyvsp[(7) - (7)].dtype).val) {
                    Setattr((yyval.p),"value",(yyvsp[(7) - (7)].dtype).val);
                  }
                }
    break;

  case 237:
#line 4733 "parser.y"
    {
		  SwigType *t = NewString("v(...)");
		  (yyval.p) = NewParmWithoutFileLineInfo(t, 0);
		  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
		}
    break;

  case 238:
#line 4741 "parser.y"
    {
                 Parm *p;
		 (yyval.p) = (yyvsp[(1) - (1)].p);
		 p = (yyvsp[(1) - (1)].p);
                 while (p) {
		   if (Getattr(p,"type")) {
		     Replace(Getattr(p,"type"),"typename ", "", DOH_REPLACE_ANY);
		   }
		   p = nextSibling(p);
                 }
               }
    break;

  case 239:
#line 4754 "parser.y"
    {
                  set_nextSibling((yyvsp[(1) - (2)].p),(yyvsp[(2) - (2)].p));
                  (yyval.p) = (yyvsp[(1) - (2)].p);
		}
    break;

  case 240:
#line 4758 "parser.y"
    { (yyval.p) = 0; }
    break;

  case 241:
#line 4761 "parser.y"
    {
                 set_nextSibling((yyvsp[(2) - (3)].p),(yyvsp[(3) - (3)].p));
		 (yyval.p) = (yyvsp[(2) - (3)].p);
                }
    break;

  case 242:
#line 4765 "parser.y"
    { (yyval.p) = 0; }
    break;

  case 243:
#line 4769 "parser.y"
    {
		  (yyval.p) = (yyvsp[(1) - (1)].p);
		  {
		    /* We need to make a possible adjustment for integer parameters. */
		    SwigType *type;
		    Node     *n = 0;

		    while (!n) {
		      type = Getattr((yyvsp[(1) - (1)].p),"type");
		      n = Swig_symbol_clookup(type,0);     /* See if we can find a node that matches the typename */
		      if ((n) && (Strcmp(nodeType(n),"cdecl") == 0)) {
			SwigType *decl = Getattr(n,"decl");
			if (!SwigType_isfunction(decl)) {
			  String *value = Getattr(n,"value");
			  if (value) {
			    String *v = Copy(value);
			    Setattr((yyvsp[(1) - (1)].p),"type",v);
			    Delete(v);
			    n = 0;
			  }
			}
		      } else {
			break;
		      }
		    }
		  }

               }
    break;

  case 244:
#line 4797 "parser.y"
    {
                  (yyval.p) = NewParmWithoutFileLineInfo(0,0);
                  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
		  Setattr((yyval.p),"value",(yyvsp[(1) - (1)].dtype).val);
               }
    break;

  case 245:
#line 4805 "parser.y"
    { 
                  (yyval.dtype) = (yyvsp[(2) - (2)].dtype); 
		  if ((yyvsp[(2) - (2)].dtype).type == T_ERROR) {
		    Swig_warning(WARN_PARSE_BAD_DEFAULT,cparse_file, cparse_line, "Can't set default argument (ignored)\n");
		    (yyval.dtype).val = 0;
		    (yyval.dtype).rawval = 0;
		    (yyval.dtype).bitfield = 0;
		    (yyval.dtype).throws = 0;
		    (yyval.dtype).throwf = 0;
		  }
               }
    break;

  case 246:
#line 4816 "parser.y"
    { 
		  (yyval.dtype) = (yyvsp[(2) - (5)].dtype);
		  if ((yyvsp[(2) - (5)].dtype).type == T_ERROR) {
		    Swig_warning(WARN_PARSE_BAD_DEFAULT,cparse_file, cparse_line, "Can't set default argument (ignored)\n");
		    (yyval.dtype) = (yyvsp[(2) - (5)].dtype);
		    (yyval.dtype).val = 0;
		    (yyval.dtype).rawval = 0;
		    (yyval.dtype).bitfield = 0;
		    (yyval.dtype).throws = 0;
		    (yyval.dtype).throwf = 0;
		  } else {
		    (yyval.dtype).val = NewStringf("%s[%s]",(yyvsp[(2) - (5)].dtype).val,(yyvsp[(4) - (5)].dtype).val); 
		  }		  
               }
    break;

  case 247:
#line 4830 "parser.y"
    {
		 skip_balanced('{','}');
		 (yyval.dtype).val = 0;
		 (yyval.dtype).rawval = 0;
                 (yyval.dtype).type = T_INT;
		 (yyval.dtype).bitfield = 0;
		 (yyval.dtype).throws = 0;
		 (yyval.dtype).throwf = 0;
	       }
    break;

  case 248:
#line 4839 "parser.y"
    { 
		 (yyval.dtype).val = 0;
		 (yyval.dtype).rawval = 0;
		 (yyval.dtype).type = 0;
		 (yyval.dtype).bitfield = (yyvsp[(2) - (2)].dtype).val;
		 (yyval.dtype).throws = 0;
		 (yyval.dtype).throwf = 0;
	       }
    break;

  case 249:
#line 4847 "parser.y"
    {
                 (yyval.dtype).val = 0;
                 (yyval.dtype).rawval = 0;
                 (yyval.dtype).type = T_INT;
		 (yyval.dtype).bitfield = 0;
		 (yyval.dtype).throws = 0;
		 (yyval.dtype).throwf = 0;
               }
    break;

  case 250:
#line 4857 "parser.y"
    {
                 (yyval.decl) = (yyvsp[(1) - (2)].decl);
		 (yyval.decl).defarg = (yyvsp[(2) - (2)].dtype).rawval ? (yyvsp[(2) - (2)].dtype).rawval : (yyvsp[(2) - (2)].dtype).val;
            }
    break;

  case 251:
#line 4861 "parser.y"
    {
              (yyval.decl) = (yyvsp[(1) - (2)].decl);
	      (yyval.decl).defarg = (yyvsp[(2) - (2)].dtype).rawval ? (yyvsp[(2) - (2)].dtype).rawval : (yyvsp[(2) - (2)].dtype).val;
            }
    break;

  case 252:
#line 4865 "parser.y"
    {
   	      (yyval.decl).type = 0;
              (yyval.decl).id = 0;
	      (yyval.decl).defarg = (yyvsp[(1) - (1)].dtype).rawval ? (yyvsp[(1) - (1)].dtype).rawval : (yyvsp[(1) - (1)].dtype).val;
            }
    break;

  case 253:
#line 4872 "parser.y"
    {
                 (yyval.decl) = (yyvsp[(1) - (1)].decl);
		 if (SwigType_isfunction((yyvsp[(1) - (1)].decl).type)) {
		   Delete(SwigType_pop_function((yyvsp[(1) - (1)].decl).type));
		 } else if (SwigType_isarray((yyvsp[(1) - (1)].decl).type)) {
		   SwigType *ta = SwigType_pop_arrays((yyvsp[(1) - (1)].decl).type);
		   if (SwigType_isfunction((yyvsp[(1) - (1)].decl).type)) {
		     Delete(SwigType_pop_function((yyvsp[(1) - (1)].decl).type));
		   } else {
		     (yyval.decl).parms = 0;
		   }
		   SwigType_push((yyvsp[(1) - (1)].decl).type,ta);
		   Delete(ta);
		 } else {
		   (yyval.decl).parms = 0;
		 }
            }
    break;

  case 254:
#line 4889 "parser.y"
    {
              (yyval.decl) = (yyvsp[(1) - (1)].decl);
	      if (SwigType_isfunction((yyvsp[(1) - (1)].decl).type)) {
		Delete(SwigType_pop_function((yyvsp[(1) - (1)].decl).type));
	      } else if (SwigType_isarray((yyvsp[(1) - (1)].decl).type)) {
		SwigType *ta = SwigType_pop_arrays((yyvsp[(1) - (1)].decl).type);
		if (SwigType_isfunction((yyvsp[(1) - (1)].decl).type)) {
		  Delete(SwigType_pop_function((yyvsp[(1) - (1)].decl).type));
		} else {
		  (yyval.decl).parms = 0;
		}
		SwigType_push((yyvsp[(1) - (1)].decl).type,ta);
		Delete(ta);
	      } else {
		(yyval.decl).parms = 0;
	      }
            }
    break;

  case 255:
#line 4906 "parser.y"
    {
   	      (yyval.decl).type = 0;
              (yyval.decl).id = 0;
	      (yyval.decl).parms = 0;
	      }
    break;

  case 256:
#line 4914 "parser.y"
    {
              (yyval.decl) = (yyvsp[(2) - (2)].decl);
	      if ((yyval.decl).type) {
		SwigType_push((yyvsp[(1) - (2)].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[(1) - (2)].type);
           }
    break;

  case 257:
#line 4922 "parser.y"
    {
              (yyval.decl) = (yyvsp[(3) - (3)].decl);
	      SwigType_add_reference((yyvsp[(1) - (3)].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[(1) - (3)].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[(1) - (3)].type);
           }
    break;

  case 258:
#line 4931 "parser.y"
    {
              (yyval.decl) = (yyvsp[(1) - (1)].decl);
	      if (!(yyval.decl).type) (yyval.decl).type = NewStringEmpty();
           }
    break;

  case 259:
#line 4935 "parser.y"
    { 
	     (yyval.decl) = (yyvsp[(2) - (2)].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_reference((yyval.decl).type);
	     if ((yyvsp[(2) - (2)].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[(2) - (2)].decl).type);
	       Delete((yyvsp[(2) - (2)].decl).type);
	     }
           }
    break;

  case 260:
#line 4944 "parser.y"
    { 
	     SwigType *t = NewStringEmpty();

	     (yyval.decl) = (yyvsp[(3) - (3)].decl);
	     SwigType_add_memberpointer(t,(yyvsp[(1) - (3)].str));
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = t;
	     }
    break;

  case 261:
#line 4955 "parser.y"
    { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[(4) - (4)].decl);
	     SwigType_add_memberpointer(t,(yyvsp[(2) - (4)].str));
	     SwigType_push((yyvsp[(1) - (4)].type),t);
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[(1) - (4)].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[(1) - (4)].type);
	     Delete(t);
	   }
    break;

  case 262:
#line 4967 "parser.y"
    { 
	     (yyval.decl) = (yyvsp[(5) - (5)].decl);
	     SwigType_add_memberpointer((yyvsp[(1) - (5)].type),(yyvsp[(2) - (5)].str));
	     SwigType_add_reference((yyvsp[(1) - (5)].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[(1) - (5)].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[(1) - (5)].type);
	   }
    break;

  case 263:
#line 4977 "parser.y"
    { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[(4) - (4)].decl);
	     SwigType_add_memberpointer(t,(yyvsp[(1) - (4)].str));
	     SwigType_add_reference(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
    break;

  case 264:
#line 4990 "parser.y"
    {
                /* Note: This is non-standard C.  Template declarator is allowed to follow an identifier */
                 (yyval.decl).id = Char((yyvsp[(1) - (1)].str));
		 (yyval.decl).type = 0;
		 (yyval.decl).parms = 0;
		 (yyval.decl).have_parms = 0;
                  }
    break;

  case 265:
#line 4997 "parser.y"
    {
                  (yyval.decl).id = Char(NewStringf("~%s",(yyvsp[(2) - (2)].str)));
                  (yyval.decl).type = 0;
                  (yyval.decl).parms = 0;
                  (yyval.decl).have_parms = 0;
                  }
    break;

  case 266:
#line 5005 "parser.y"
    {
                  (yyval.decl).id = Char((yyvsp[(2) - (3)].str));
                  (yyval.decl).type = 0;
                  (yyval.decl).parms = 0;
                  (yyval.decl).have_parms = 0;
                  }
    break;

  case 267:
#line 5021 "parser.y"
    {
		    (yyval.decl) = (yyvsp[(3) - (4)].decl);
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[(2) - (4)].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[(2) - (4)].type);
                  }
    break;

  case 268:
#line 5029 "parser.y"
    {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(4) - (5)].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t,(yyvsp[(2) - (5)].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		    }
    break;

  case 269:
#line 5040 "parser.y"
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(1) - (3)].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(char*)"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
    break;

  case 270:
#line 5051 "parser.y"
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(1) - (4)].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[(3) - (4)].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
    break;

  case 271:
#line 5062 "parser.y"
    {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[(1) - (4)].decl);
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[(3) - (4)].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[(3) - (4)].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		  }
    break;

  case 272:
#line 5081 "parser.y"
    {
                /* Note: This is non-standard C.  Template declarator is allowed to follow an identifier */
                 (yyval.decl).id = Char((yyvsp[(1) - (1)].str));
		 (yyval.decl).type = 0;
		 (yyval.decl).parms = 0;
		 (yyval.decl).have_parms = 0;
                  }
    break;

  case 273:
#line 5089 "parser.y"
    {
                  (yyval.decl).id = Char(NewStringf("~%s",(yyvsp[(2) - (2)].str)));
                  (yyval.decl).type = 0;
                  (yyval.decl).parms = 0;
                  (yyval.decl).have_parms = 0;
                  }
    break;

  case 274:
#line 5106 "parser.y"
    {
		    (yyval.decl) = (yyvsp[(3) - (4)].decl);
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[(2) - (4)].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[(2) - (4)].type);
                  }
    break;

  case 275:
#line 5114 "parser.y"
    {
                    (yyval.decl) = (yyvsp[(3) - (4)].decl);
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = NewStringEmpty();
		    }
		    SwigType_add_reference((yyval.decl).type);
                  }
    break;

  case 276:
#line 5121 "parser.y"
    {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(4) - (5)].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t,(yyvsp[(2) - (5)].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		    }
    break;

  case 277:
#line 5132 "parser.y"
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(1) - (3)].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(char*)"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
    break;

  case 278:
#line 5143 "parser.y"
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(1) - (4)].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[(3) - (4)].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
    break;

  case 279:
#line 5154 "parser.y"
    {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[(1) - (4)].decl);
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[(3) - (4)].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[(3) - (4)].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		  }
    break;

  case 280:
#line 5173 "parser.y"
    {
		    (yyval.decl).type = (yyvsp[(1) - (1)].type);
                    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
                  }
    break;

  case 281:
#line 5179 "parser.y"
    { 
                     (yyval.decl) = (yyvsp[(2) - (2)].decl);
                     SwigType_push((yyvsp[(1) - (2)].type),(yyvsp[(2) - (2)].decl).type);
		     (yyval.decl).type = (yyvsp[(1) - (2)].type);
		     Delete((yyvsp[(2) - (2)].decl).type);
                  }
    break;

  case 282:
#line 5185 "parser.y"
    {
		    (yyval.decl).type = (yyvsp[(1) - (2)].type);
		    SwigType_add_reference((yyval.decl).type);
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		  }
    break;

  case 283:
#line 5192 "parser.y"
    {
		    (yyval.decl) = (yyvsp[(3) - (3)].decl);
		    SwigType_add_reference((yyvsp[(1) - (3)].type));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[(1) - (3)].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[(1) - (3)].type);
                  }
    break;

  case 284:
#line 5201 "parser.y"
    {
		    (yyval.decl) = (yyvsp[(1) - (1)].decl);
                  }
    break;

  case 285:
#line 5204 "parser.y"
    {
		    (yyval.decl) = (yyvsp[(2) - (2)].decl);
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_reference((yyval.decl).type);
		    if ((yyvsp[(2) - (2)].decl).type) {
		      SwigType_push((yyval.decl).type,(yyvsp[(2) - (2)].decl).type);
		      Delete((yyvsp[(2) - (2)].decl).type);
		    }
                  }
    break;

  case 286:
#line 5213 "parser.y"
    { 
                    (yyval.decl).id = 0;
                    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
                    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_reference((yyval.decl).type);
                  }
    break;

  case 287:
#line 5220 "parser.y"
    { 
		    (yyval.decl).type = NewStringEmpty();
                    SwigType_add_memberpointer((yyval.decl).type,(yyvsp[(1) - (2)].str));
                    (yyval.decl).id = 0;
                    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
      	          }
    break;

  case 288:
#line 5227 "parser.y"
    { 
		    SwigType *t = NewStringEmpty();
                    (yyval.decl).type = (yyvsp[(1) - (3)].type);
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		    SwigType_add_memberpointer(t,(yyvsp[(2) - (3)].str));
		    SwigType_push((yyval.decl).type,t);
		    Delete(t);
                  }
    break;

  case 289:
#line 5237 "parser.y"
    { 
		    (yyval.decl) = (yyvsp[(4) - (4)].decl);
		    SwigType_add_memberpointer((yyvsp[(1) - (4)].type),(yyvsp[(2) - (4)].str));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[(1) - (4)].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[(1) - (4)].type);
                  }
    break;

  case 290:
#line 5248 "parser.y"
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(1) - (3)].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(char*)"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
    break;

  case 291:
#line 5259 "parser.y"
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[(1) - (4)].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[(3) - (4)].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
    break;

  case 292:
#line 5270 "parser.y"
    { 
		    (yyval.decl).type = NewStringEmpty();
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		    SwigType_add_array((yyval.decl).type,(char*)"");
                  }
    break;

  case 293:
#line 5277 "parser.y"
    { 
		    (yyval.decl).type = NewStringEmpty();
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		    SwigType_add_array((yyval.decl).type,(yyvsp[(2) - (3)].dtype).val);
		  }
    break;

  case 294:
#line 5284 "parser.y"
    {
                    (yyval.decl) = (yyvsp[(2) - (3)].decl);
		  }
    break;

  case 295:
#line 5287 "parser.y"
    {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[(1) - (4)].decl);
		    t = NewStringEmpty();
                    SwigType_add_function(t,(yyvsp[(3) - (4)].pl));
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[(3) - (4)].pl);
		      (yyval.decl).have_parms = 1;
		    }
		  }
    break;

  case 296:
#line 5304 "parser.y"
    {
                    (yyval.decl).type = NewStringEmpty();
                    SwigType_add_function((yyval.decl).type,(yyvsp[(2) - (3)].pl));
		    (yyval.decl).parms = (yyvsp[(2) - (3)].pl);
		    (yyval.decl).have_parms = 1;
		    (yyval.decl).id = 0;
                  }
    break;

  case 297:
#line 5314 "parser.y"
    { 
               (yyval.type) = NewStringEmpty();
               SwigType_add_pointer((yyval.type));
	       SwigType_push((yyval.type),(yyvsp[(2) - (3)].str));
	       SwigType_push((yyval.type),(yyvsp[(3) - (3)].type));
	       Delete((yyvsp[(3) - (3)].type));
           }
    break;

  case 298:
#line 5321 "parser.y"
    {
	     (yyval.type) = NewStringEmpty();
	     SwigType_add_pointer((yyval.type));
	     SwigType_push((yyval.type),(yyvsp[(2) - (2)].type));
	     Delete((yyvsp[(2) - (2)].type));
	     }
    break;

  case 299:
#line 5327 "parser.y"
    { 
	     	(yyval.type) = NewStringEmpty();	
		SwigType_add_pointer((yyval.type));
	        SwigType_push((yyval.type),(yyvsp[(2) - (2)].str));
           }
    break;

  case 300:
#line 5332 "parser.y"
    {
	      (yyval.type) = NewStringEmpty();
	      SwigType_add_pointer((yyval.type));
           }
    break;

  case 301:
#line 5338 "parser.y"
    {
	          (yyval.str) = NewStringEmpty();
	          if ((yyvsp[(1) - (1)].id)) SwigType_add_qualifier((yyval.str),(yyvsp[(1) - (1)].id));
               }
    break;

  case 302:
#line 5342 "parser.y"
    {
		  (yyval.str) = (yyvsp[(2) - (2)].str);
	          if ((yyvsp[(1) - (2)].id)) SwigType_add_qualifier((yyval.str),(yyvsp[(1) - (2)].id));
               }
    break;

  case 303:
#line 5348 "parser.y"
    { (yyval.id) = "const"; }
    break;

  case 304:
#line 5349 "parser.y"
    { (yyval.id) = "volatile"; }
    break;

  case 305:
#line 5350 "parser.y"
    { (yyval.id) = 0; }
    break;

  case 306:
#line 5356 "parser.y"
    {
                   (yyval.type) = (yyvsp[(1) - (1)].type);
                   Replace((yyval.type),"typename ","", DOH_REPLACE_ANY);
                }
    break;

  case 307:
#line 5362 "parser.y"
    {
                   (yyval.type) = (yyvsp[(2) - (2)].type);
	           SwigType_push((yyval.type),(yyvsp[(1) - (2)].str));
               }
    break;

  case 308:
#line 5366 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type); }
    break;

  case 309:
#line 5367 "parser.y"
    {
		  (yyval.type) = (yyvsp[(1) - (2)].type);
	          SwigType_push((yyval.type),(yyvsp[(2) - (2)].str));
	       }
    break;

  case 310:
#line 5371 "parser.y"
    {
		  (yyval.type) = (yyvsp[(2) - (3)].type);
	          SwigType_push((yyval.type),(yyvsp[(3) - (3)].str));
	          SwigType_push((yyval.type),(yyvsp[(1) - (3)].str));
	       }
    break;

  case 311:
#line 5378 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type);
                  /* Printf(stdout,"primitive = '%s'\n", $$);*/
                }
    break;

  case 312:
#line 5381 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type); }
    break;

  case 313:
#line 5382 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type); }
    break;

  case 314:
#line 5383 "parser.y"
    { (yyval.type) = NewStringf("%s%s",(yyvsp[(1) - (2)].type),(yyvsp[(2) - (2)].id)); }
    break;

  case 315:
#line 5384 "parser.y"
    { (yyval.type) = NewStringf("enum %s", (yyvsp[(2) - (2)].str)); }
    break;

  case 316:
#line 5385 "parser.y"
    { (yyval.type) = (yyvsp[(1) - (1)].type); }
    break;

  case 317:
#line 5387 "parser.y"
    {
		  (yyval.type) = (yyvsp[(1) - (1)].str);
               }
    break;

  case 318:
#line 5390 "parser.y"
    { 
		 (yyval.type) = NewStringf("%s %s", (yyvsp[(1) - (2)].id), (yyvsp[(2) - (2)].str));
               }
    break;

  case 319:
#line 5395 "parser.y"
    {
		 if (!(yyvsp[(1) - (1)].ptype).type) (yyvsp[(1) - (1)].ptype).type = NewString("int");
		 if ((yyvsp[(1) - (1)].ptype).us) {
		   (yyval.type) = NewStringf("%s %s", (yyvsp[(1) - (1)].ptype).us, (yyvsp[(1) - (1)].ptype).type);
		   Delete((yyvsp[(1) - (1)].ptype).us);
                   Delete((yyvsp[(1) - (1)].ptype).type);
		 } else {
                   (yyval.type) = (yyvsp[(1) - (1)].ptype).type;
		 }
		 if (Cmp((yyval.type),"signed int") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("int");
                 } else if (Cmp((yyval.type),"signed long") == 0) {
		   Delete((yyval.type));
                   (yyval.type) = NewString("long");
                 } else if (Cmp((yyval.type),"signed short") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("short");
		 } else if (Cmp((yyval.type),"signed long long") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("long long");
		 }
               }
    break;

  case 320:
#line 5420 "parser.y"
    { 
                 (yyval.ptype) = (yyvsp[(1) - (1)].ptype);
               }
    break;

  case 321:
#line 5423 "parser.y"
    {
                    if ((yyvsp[(1) - (2)].ptype).us && (yyvsp[(2) - (2)].ptype).us) {
		      Swig_error(cparse_file, cparse_line, "Extra %s specifier.\n", (yyvsp[(2) - (2)].ptype).us);
		    }
                    (yyval.ptype) = (yyvsp[(2) - (2)].ptype);
                    if ((yyvsp[(1) - (2)].ptype).us) (yyval.ptype).us = (yyvsp[(1) - (2)].ptype).us;
		    if ((yyvsp[(1) - (2)].ptype).type) {
		      if (!(yyvsp[(2) - (2)].ptype).type) (yyval.ptype).type = (yyvsp[(1) - (2)].ptype).type;
		      else {
			int err = 0;
			if ((Cmp((yyvsp[(1) - (2)].ptype).type,"long") == 0)) {
			  if ((Cmp((yyvsp[(2) - (2)].ptype).type,"long") == 0) || (Strncmp((yyvsp[(2) - (2)].ptype).type,"double",6) == 0)) {
			    (yyval.ptype).type = NewStringf("long %s", (yyvsp[(2) - (2)].ptype).type);
			  } else if (Cmp((yyvsp[(2) - (2)].ptype).type,"int") == 0) {
			    (yyval.ptype).type = (yyvsp[(1) - (2)].ptype).type;
			  } else {
			    err = 1;
			  }
			} else if ((Cmp((yyvsp[(1) - (2)].ptype).type,"short")) == 0) {
			  if (Cmp((yyvsp[(2) - (2)].ptype).type,"int") == 0) {
			    (yyval.ptype).type = (yyvsp[(1) - (2)].ptype).type;
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[(1) - (2)].ptype).type,"int") == 0) {
			  (yyval.ptype).type = (yyvsp[(2) - (2)].ptype).type;
			} else if (Cmp((yyvsp[(1) - (2)].ptype).type,"double") == 0) {
			  if (Cmp((yyvsp[(2) - (2)].ptype).type,"long") == 0) {
			    (yyval.ptype).type = NewString("long double");
			  } else if (Cmp((yyvsp[(2) - (2)].ptype).type,"complex") == 0) {
			    (yyval.ptype).type = NewString("double complex");
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[(1) - (2)].ptype).type,"float") == 0) {
			  if (Cmp((yyvsp[(2) - (2)].ptype).type,"complex") == 0) {
			    (yyval.ptype).type = NewString("float complex");
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[(1) - (2)].ptype).type,"complex") == 0) {
			  (yyval.ptype).type = NewStringf("%s complex", (yyvsp[(2) - (2)].ptype).type);
			} else {
			  err = 1;
			}
			if (err) {
			  Swig_error(cparse_file, cparse_line, "Extra %s specifier.\n", (yyvsp[(1) - (2)].ptype).type);
			}
		      }
		    }
               }
    break;

  case 322:
#line 5477 "parser.y"
    { 
		    (yyval.ptype).type = NewString("int");
                    (yyval.ptype).us = 0;
               }
    break;

  case 323:
#line 5481 "parser.y"
    { 
                    (yyval.ptype).type = NewString("short");
                    (yyval.ptype).us = 0;
                }
    break;

  case 324:
#line 5485 "parser.y"
    { 
                    (yyval.ptype).type = NewString("long");
                    (yyval.ptype).us = 0;
                }
    break;

  case 325:
#line 5489 "parser.y"
    { 
                    (yyval.ptype).type = NewString("char");
                    (yyval.ptype).us = 0;
                }
    break;

  case 326:
#line 5493 "parser.y"
    { 
                    (yyval.ptype).type = NewString("wchar_t");
                    (yyval.ptype).us = 0;
                }
    break;

  case 327:
#line 5497 "parser.y"
    { 
                    (yyval.ptype).type = NewString("float");
                    (yyval.ptype).us = 0;
                }
    break;

  case 328:
#line 5501 "parser.y"
    { 
                    (yyval.ptype).type = NewString("double");
                    (yyval.ptype).us = 0;
                }
    break;

  case 329:
#line 5505 "parser.y"
    { 
                    (yyval.ptype).us = NewString("signed");
                    (yyval.ptype).type = 0;
                }
    break;

  case 330:
#line 5509 "parser.y"
    { 
                    (yyval.ptype).us = NewString("unsigned");
                    (yyval.ptype).type = 0;
                }
    break;

  case 331:
#line 5513 "parser.y"
    { 
                    (yyval.ptype).type = NewString("complex");
                    (yyval.ptype).us = 0;
                }
    break;

  case 332:
#line 5517 "parser.y"
    { 
                    (yyval.ptype).type = NewString("__int8");
                    (yyval.ptype).us = 0;
                }
    break;

  case 333:
#line 5521 "parser.y"
    { 
                    (yyval.ptype).type = NewString("__int16");
                    (yyval.ptype).us = 0;
                }
    break;

  case 334:
#line 5525 "parser.y"
    { 
                    (yyval.ptype).type = NewString("__int32");
                    (yyval.ptype).us = 0;
                }
    break;

  case 335:
#line 5529 "parser.y"
    { 
                    (yyval.ptype).type = NewString("__int64");
                    (yyval.ptype).us = 0;
                }
    break;

  case 336:
#line 5535 "parser.y"
    { /* scanner_check_typedef(); */ }
    break;

  case 337:
#line 5535 "parser.y"
    {
                   (yyval.dtype) = (yyvsp[(2) - (2)].dtype);
		   if ((yyval.dtype).type == T_STRING) {
		     (yyval.dtype).rawval = NewStringf("\"%(escape)s\"",(yyval.dtype).val);
		   } else if ((yyval.dtype).type != T_CHAR) {
		     (yyval.dtype).rawval = 0;
		   }
		   (yyval.dtype).bitfield = 0;
		   (yyval.dtype).throws = 0;
		   (yyval.dtype).throwf = 0;
		   scanner_ignore_typedef();
                }
    break;

  case 338:
#line 5561 "parser.y"
    { (yyval.id) = (yyvsp[(1) - (1)].id); }
    break;

  case 339:
#line 5562 "parser.y"
    { (yyval.id) = (char *) 0;}
    break;

  case 340:
#line 5565 "parser.y"
    { 

                  /* Ignore if there is a trailing comma in the enum list */
                  if ((yyvsp[(3) - (3)].node)) {
                    Node *leftSibling = Getattr((yyvsp[(1) - (3)].node),"_last");
                    if (!leftSibling) {
                      leftSibling=(yyvsp[(1) - (3)].node);
                    }
                    set_nextSibling(leftSibling,(yyvsp[(3) - (3)].node));
                    Setattr((yyvsp[(1) - (3)].node),"_last",(yyvsp[(3) - (3)].node));
                  }
		  (yyval.node) = (yyvsp[(1) - (3)].node);
               }
    break;

  case 341:
#line 5578 "parser.y"
    { 
                   (yyval.node) = (yyvsp[(1) - (1)].node); 
                   if ((yyvsp[(1) - (1)].node)) {
                     Setattr((yyvsp[(1) - (1)].node),"_last",(yyvsp[(1) - (1)].node));
                   }
               }
    break;

  case 342:
#line 5586 "parser.y"
    {
		   SwigType *type = NewSwigType(T_INT);
		   (yyval.node) = new_node("enumitem");
		   Setattr((yyval.node),"name",(yyvsp[(1) - (1)].id));
		   Setattr((yyval.node),"type",type);
		   SetFlag((yyval.node),"feature:immutable");
		   Delete(type);
		 }
    break;

  case 343:
#line 5594 "parser.y"
    {
		   (yyval.node) = new_node("enumitem");
		   Setattr((yyval.node),"name",(yyvsp[(1) - (3)].id));
		   Setattr((yyval.node),"enumvalue", (yyvsp[(3) - (3)].dtype).val);
	           if ((yyvsp[(3) - (3)].dtype).type == T_CHAR) {
		     SwigType *type = NewSwigType(T_CHAR);
		     Setattr((yyval.node),"value",NewStringf("\'%(escape)s\'", (yyvsp[(3) - (3)].dtype).val));
		     Setattr((yyval.node),"type",type);
		     Delete(type);
		   } else {
		     SwigType *type = NewSwigType((yyvsp[(3) - (3)].dtype).type == T_BOOL ? T_BOOL : T_INT);
		     Setattr((yyval.node),"value",(yyvsp[(1) - (3)].id));
		     Setattr((yyval.node),"type",type);
		     Delete(type);
		   }
		   SetFlag((yyval.node),"feature:immutable");
                 }
    break;

  case 344:
#line 5611 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 345:
#line 5614 "parser.y"
    {
                   (yyval.dtype) = (yyvsp[(1) - (1)].dtype);
		   if (((yyval.dtype).type != T_INT) && ((yyval.dtype).type != T_UINT) &&
		       ((yyval.dtype).type != T_LONG) && ((yyval.dtype).type != T_ULONG) &&
		       ((yyval.dtype).type != T_SHORT) && ((yyval.dtype).type != T_USHORT) &&
		       ((yyval.dtype).type != T_SCHAR) && ((yyval.dtype).type != T_UCHAR) &&
		       ((yyval.dtype).type != T_CHAR) && ((yyval.dtype).type != T_BOOL)) {
		     Swig_error(cparse_file,cparse_line,"Type error. Expecting an integral type\n");
		   }
		   if ((yyval.dtype).type == T_CHAR) (yyval.dtype).type = T_INT;
                }
    break;

  case 346:
#line 5629 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 347:
#line 5630 "parser.y"
    {
		 Node *n;
		 (yyval.dtype).val = (yyvsp[(1) - (1)].type);
		 (yyval.dtype).type = T_INT;
		 /* Check if value is in scope */
		 n = Swig_symbol_clookup((yyvsp[(1) - (1)].type),0);
		 if (n) {
                   /* A band-aid for enum values used in expressions. */
                   if (Strcmp(nodeType(n),"enumitem") == 0) {
                     String *q = Swig_symbol_qualified(n);
                     if (q) {
                       (yyval.dtype).val = NewStringf("%s::%s", q, Getattr(n,"name"));
                       Delete(q);
                     }
                   }
		 }
               }
    break;

  case 348:
#line 5649 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 349:
#line 5650 "parser.y"
    {
		    (yyval.dtype).val = NewString((yyvsp[(1) - (1)].id));
                    (yyval.dtype).type = T_STRING;
               }
    break;

  case 350:
#line 5654 "parser.y"
    {
		  SwigType_push((yyvsp[(3) - (5)].type),(yyvsp[(4) - (5)].decl).type);
		  (yyval.dtype).val = NewStringf("sizeof(%s)",SwigType_str((yyvsp[(3) - (5)].type),0));
		  (yyval.dtype).type = T_ULONG;
               }
    break;

  case 351:
#line 5659 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 352:
#line 5660 "parser.y"
    {
		  (yyval.dtype).val = NewString((yyvsp[(1) - (1)].str));
		  if (Len((yyval.dtype).val)) {
		    (yyval.dtype).rawval = NewStringf("'%(escape)s'", (yyval.dtype).val);
		  } else {
		    (yyval.dtype).rawval = NewString("'\\0'");
		  }
		  (yyval.dtype).type = T_CHAR;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
	       }
    break;

  case 353:
#line 5674 "parser.y"
    {
   	            (yyval.dtype).val = NewStringf("(%s)",(yyvsp[(2) - (3)].dtype).val);
		    (yyval.dtype).type = (yyvsp[(2) - (3)].dtype).type;
   	       }
    break;

  case 354:
#line 5681 "parser.y"
    {
                 (yyval.dtype) = (yyvsp[(4) - (4)].dtype);
		 if ((yyvsp[(4) - (4)].dtype).type != T_STRING) {
		   switch ((yyvsp[(2) - (4)].dtype).type) {
		     case T_FLOAT:
		     case T_DOUBLE:
		     case T_LONGDOUBLE:
		     case T_FLTCPLX:
		     case T_DBLCPLX:
		       (yyval.dtype).val = NewStringf("(%s)%s", (yyvsp[(2) - (4)].dtype).val, (yyvsp[(4) - (4)].dtype).val); /* SwigType_str and decimal points don't mix! */
		       break;
		     default:
		       (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[(2) - (4)].dtype).val,0), (yyvsp[(4) - (4)].dtype).val);
		       break;
		   }
		 }
 	       }
    break;

  case 355:
#line 5698 "parser.y"
    {
                 (yyval.dtype) = (yyvsp[(5) - (5)].dtype);
		 if ((yyvsp[(5) - (5)].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[(2) - (5)].dtype).val,(yyvsp[(3) - (5)].type));
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[(2) - (5)].dtype).val,0), (yyvsp[(5) - (5)].dtype).val);
		 }
 	       }
    break;

  case 356:
#line 5705 "parser.y"
    {
                 (yyval.dtype) = (yyvsp[(5) - (5)].dtype);
		 if ((yyvsp[(5) - (5)].dtype).type != T_STRING) {
		   SwigType_add_reference((yyvsp[(2) - (5)].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[(2) - (5)].dtype).val,0), (yyvsp[(5) - (5)].dtype).val);
		 }
 	       }
    break;

  case 357:
#line 5712 "parser.y"
    {
                 (yyval.dtype) = (yyvsp[(6) - (6)].dtype);
		 if ((yyvsp[(6) - (6)].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[(2) - (6)].dtype).val,(yyvsp[(3) - (6)].type));
		   SwigType_add_reference((yyvsp[(2) - (6)].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[(2) - (6)].dtype).val,0), (yyvsp[(6) - (6)].dtype).val);
		 }
 	       }
    break;

  case 358:
#line 5720 "parser.y"
    {
		 (yyval.dtype) = (yyvsp[(2) - (2)].dtype);
                 (yyval.dtype).val = NewStringf("&%s",(yyvsp[(2) - (2)].dtype).val);
	       }
    break;

  case 359:
#line 5724 "parser.y"
    {
		 (yyval.dtype) = (yyvsp[(2) - (2)].dtype);
                 (yyval.dtype).val = NewStringf("*%s",(yyvsp[(2) - (2)].dtype).val);
	       }
    break;

  case 360:
#line 5730 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 361:
#line 5731 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 362:
#line 5732 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 363:
#line 5733 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 364:
#line 5734 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 365:
#line 5735 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 366:
#line 5736 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 367:
#line 5737 "parser.y"
    { (yyval.dtype) = (yyvsp[(1) - (1)].dtype); }
    break;

  case 368:
#line 5740 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s+%s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 369:
#line 5744 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s-%s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 370:
#line 5748 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s*%s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 371:
#line 5752 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s/%s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 372:
#line 5756 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s%%%s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 373:
#line 5760 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s&%s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 374:
#line 5764 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s|%s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 375:
#line 5768 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s^%s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[(1) - (3)].dtype).type,(yyvsp[(3) - (3)].dtype).type);
	       }
    break;

  case 376:
#line 5772 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s << %s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[(1) - (3)].dtype).type);
	       }
    break;

  case 377:
#line 5776 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s >> %s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[(1) - (3)].dtype).type);
	       }
    break;

  case 378:
#line 5780 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s&&%s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
    break;

  case 379:
#line 5784 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s||%s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
    break;

  case 380:
#line 5788 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s==%s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
    break;

  case 381:
#line 5792 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s!=%s",(yyvsp[(1) - (3)].dtype).val,(yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
    break;

  case 382:
#line 5806 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s >= %s", (yyvsp[(1) - (3)].dtype).val, (yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
    break;

  case 383:
#line 5810 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s <= %s", (yyvsp[(1) - (3)].dtype).val, (yyvsp[(3) - (3)].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
    break;

  case 384:
#line 5814 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("%s?%s:%s", (yyvsp[(1) - (5)].dtype).val, (yyvsp[(3) - (5)].dtype).val, (yyvsp[(5) - (5)].dtype).val);
		 /* This may not be exactly right, but is probably good enough
		  * for the purposes of parsing constant expressions. */
		 (yyval.dtype).type = promote((yyvsp[(3) - (5)].dtype).type, (yyvsp[(5) - (5)].dtype).type);
	       }
    break;

  case 385:
#line 5820 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("-%s",(yyvsp[(2) - (2)].dtype).val);
		 (yyval.dtype).type = (yyvsp[(2) - (2)].dtype).type;
	       }
    break;

  case 386:
#line 5824 "parser.y"
    {
                 (yyval.dtype).val = NewStringf("+%s",(yyvsp[(2) - (2)].dtype).val);
		 (yyval.dtype).type = (yyvsp[(2) - (2)].dtype).type;
	       }
    break;

  case 387:
#line 5828 "parser.y"
    {
		 (yyval.dtype).val = NewStringf("~%s",(yyvsp[(2) - (2)].dtype).val);
		 (yyval.dtype).type = (yyvsp[(2) - (2)].dtype).type;
	       }
    break;

  case 388:
#line 5832 "parser.y"
    {
                 (yyval.dtype).val = NewStringf("!%s",(yyvsp[(2) - (2)].dtype).val);
		 (yyval.dtype).type = T_INT;
	       }
    break;

  case 389:
#line 5836 "parser.y"
    {
		 String *qty;
                 skip_balanced('(',')');
		 qty = Swig_symbol_type_qualify((yyvsp[(1) - (2)].type),0);
		 if (SwigType_istemplate(qty)) {
		   String *nstr = SwigType_namestr(qty);
		   Delete(qty);
		   qty = nstr;
		 }
		 (yyval.dtype).val = NewStringf("%s%s",qty,scanner_ccode);
		 Clear(scanner_ccode);
		 (yyval.dtype).type = T_INT;
		 Delete(qty);
               }
    break;

  case 390:
#line 5852 "parser.y"
    {
		 (yyval.bases) = (yyvsp[(1) - (1)].bases);
               }
    break;

  case 391:
#line 5857 "parser.y"
    { inherit_list = 1; }
    break;

  case 392:
#line 5857 "parser.y"
    { (yyval.bases) = (yyvsp[(3) - (3)].bases); inherit_list = 0; }
    break;

  case 393:
#line 5858 "parser.y"
    { (yyval.bases) = 0; }
    break;

  case 394:
#line 5861 "parser.y"
    {
		   Hash *list = NewHash();
		   Node *base = (yyvsp[(1) - (1)].node);
		   Node *name = Getattr(base,"name");
		   List *lpublic = NewList();
		   List *lprotected = NewList();
		   List *lprivate = NewList();
		   Setattr(list,"public",lpublic);
		   Setattr(list,"protected",lprotected);
		   Setattr(list,"private",lprivate);
		   Delete(lpublic);
		   Delete(lprotected);
		   Delete(lprivate);
		   Append(Getattr(list,Getattr(base,"access")),name);
	           (yyval.bases) = list;
               }
    break;

  case 395:
#line 5878 "parser.y"
    {
		   Hash *list = (yyvsp[(1) - (3)].bases);
		   Node *base = (yyvsp[(3) - (3)].node);
		   Node *name = Getattr(base,"name");
		   Append(Getattr(list,Getattr(base,"access")),name);
                   (yyval.bases) = list;
               }
    break;

  case 396:
#line 5887 "parser.y"
    {
		 (yyval.node) = NewHash();
		 Setfile((yyval.node),cparse_file);
		 Setline((yyval.node),cparse_line);
		 Setattr((yyval.node),"name",(yyvsp[(2) - (2)].str));
                 if (last_cpptype && (Strcmp(last_cpptype,"struct") != 0)) {
		   Setattr((yyval.node),"access","private");
		   Swig_warning(WARN_PARSE_NO_ACCESS,cparse_file,cparse_line,
				"No access specifier given for base class %s (ignored).\n",(yyvsp[(2) - (2)].str));
                 } else {
		   Setattr((yyval.node),"access","public");
		 }
               }
    break;

  case 397:
#line 5900 "parser.y"
    {
		 (yyval.node) = NewHash();
		 Setfile((yyval.node),cparse_file);
		 Setline((yyval.node),cparse_line);
		 Setattr((yyval.node),"name",(yyvsp[(4) - (4)].str));
		 Setattr((yyval.node),"access",(yyvsp[(2) - (4)].id));
	         if (Strcmp((yyvsp[(2) - (4)].id),"public") != 0) {
		   Swig_warning(WARN_PARSE_PRIVATE_INHERIT, cparse_file, 
				cparse_line,"%s inheritance ignored.\n", (yyvsp[(2) - (4)].id));
		 }
               }
    break;

  case 398:
#line 5913 "parser.y"
    { (yyval.id) = (char*)"public"; }
    break;

  case 399:
#line 5914 "parser.y"
    { (yyval.id) = (char*)"private"; }
    break;

  case 400:
#line 5915 "parser.y"
    { (yyval.id) = (char*)"protected"; }
    break;

  case 401:
#line 5919 "parser.y"
    { 
                   (yyval.id) = (char*)"class"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
    break;

  case 402:
#line 5923 "parser.y"
    { 
                   (yyval.id) = (char *)"typename"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
    break;

  case 403:
#line 5929 "parser.y"
    {
                 (yyval.id) = (yyvsp[(1) - (1)].id);
               }
    break;

  case 404:
#line 5932 "parser.y"
    { 
                   (yyval.id) = (char*)"struct"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
    break;

  case 405:
#line 5936 "parser.y"
    {
                   (yyval.id) = (char*)"union"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
    break;

  case 408:
#line 5946 "parser.y"
    {
                    (yyval.dtype).qualifier = (yyvsp[(1) - (1)].str);
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
               }
    break;

  case 409:
#line 5951 "parser.y"
    {
                    (yyval.dtype).qualifier = 0;
                    (yyval.dtype).throws = (yyvsp[(3) - (4)].pl);
                    (yyval.dtype).throwf = NewString("1");
               }
    break;

  case 410:
#line 5956 "parser.y"
    {
                    (yyval.dtype).qualifier = (yyvsp[(1) - (5)].str);
                    (yyval.dtype).throws = (yyvsp[(4) - (5)].pl);
                    (yyval.dtype).throwf = NewString("1");
               }
    break;

  case 411:
#line 5961 "parser.y"
    { 
                    (yyval.dtype).qualifier = 0; 
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
               }
    break;

  case 412:
#line 5968 "parser.y"
    { 
                    Clear(scanner_ccode); 
                    (yyval.decl).have_parms = 0; 
                    (yyval.decl).defarg = 0; 
		    (yyval.decl).throws = (yyvsp[(1) - (3)].dtype).throws;
		    (yyval.decl).throwf = (yyvsp[(1) - (3)].dtype).throwf;
               }
    break;

  case 413:
#line 5975 "parser.y"
    { 
                    skip_balanced('{','}'); 
                    (yyval.decl).have_parms = 0; 
                    (yyval.decl).defarg = 0; 
                    (yyval.decl).throws = (yyvsp[(1) - (3)].dtype).throws;
                    (yyval.decl).throwf = (yyvsp[(1) - (3)].dtype).throwf;
               }
    break;

  case 414:
#line 5982 "parser.y"
    { 
                    Clear(scanner_ccode); 
                    (yyval.decl).parms = (yyvsp[(2) - (4)].pl); 
                    (yyval.decl).have_parms = 1; 
                    (yyval.decl).defarg = 0; 
		    (yyval.decl).throws = 0;
		    (yyval.decl).throwf = 0;
               }
    break;

  case 415:
#line 5990 "parser.y"
    {
                    skip_balanced('{','}'); 
                    (yyval.decl).parms = (yyvsp[(2) - (4)].pl); 
                    (yyval.decl).have_parms = 1; 
                    (yyval.decl).defarg = 0; 
                    (yyval.decl).throws = 0;
                    (yyval.decl).throwf = 0;
               }
    break;

  case 416:
#line 5998 "parser.y"
    { 
                    (yyval.decl).have_parms = 0; 
                    (yyval.decl).defarg = (yyvsp[(2) - (3)].dtype).val; 
                    (yyval.decl).throws = 0;
                    (yyval.decl).throwf = 0;
               }
    break;

  case 421:
#line 6014 "parser.y"
    {
	            skip_balanced('(',')');
                    Clear(scanner_ccode);
            	}
    break;

  case 422:
#line 6020 "parser.y"
    { 
                     String *s = NewStringEmpty();
                     SwigType_add_template(s,(yyvsp[(2) - (3)].p));
                     (yyval.id) = Char(s);
		     scanner_last_id(1);
                 }
    break;

  case 423:
#line 6026 "parser.y"
    { (yyval.id) = (char*)"";  }
    break;

  case 424:
#line 6029 "parser.y"
    { (yyval.id) = (yyvsp[(1) - (1)].id); }
    break;

  case 425:
#line 6030 "parser.y"
    { (yyval.id) = (yyvsp[(1) - (1)].id); }
    break;

  case 426:
#line 6033 "parser.y"
    { (yyval.id) = (yyvsp[(1) - (1)].id); }
    break;

  case 427:
#line 6034 "parser.y"
    { (yyval.id) = 0; }
    break;

  case 428:
#line 6037 "parser.y"
    { 
                  (yyval.str) = 0;
		  if (!(yyval.str)) (yyval.str) = NewStringf("%s%s", (yyvsp[(1) - (2)].str),(yyvsp[(2) - (2)].str));
      	          Delete((yyvsp[(2) - (2)].str));
               }
    break;

  case 429:
#line 6042 "parser.y"
    { 
		 (yyval.str) = NewStringf("::%s%s",(yyvsp[(3) - (4)].str),(yyvsp[(4) - (4)].str));
                 Delete((yyvsp[(4) - (4)].str));
               }
    break;

  case 430:
#line 6046 "parser.y"
    {
		 (yyval.str) = NewString((yyvsp[(1) - (1)].str));
   	       }
    break;

  case 431:
#line 6049 "parser.y"
    {
		 (yyval.str) = NewStringf("::%s",(yyvsp[(3) - (3)].str));
               }
    break;

  case 432:
#line 6052 "parser.y"
    {
                 (yyval.str) = NewString((yyvsp[(1) - (1)].str));
	       }
    break;

  case 433:
#line 6055 "parser.y"
    {
                 (yyval.str) = NewStringf("::%s",(yyvsp[(3) - (3)].str));
               }
    break;

  case 434:
#line 6060 "parser.y"
    {
                   (yyval.str) = NewStringf("::%s%s",(yyvsp[(2) - (3)].str),(yyvsp[(3) - (3)].str));
		   Delete((yyvsp[(3) - (3)].str));
               }
    break;

  case 435:
#line 6064 "parser.y"
    {
                   (yyval.str) = NewStringf("::%s",(yyvsp[(2) - (2)].str));
               }
    break;

  case 436:
#line 6067 "parser.y"
    {
                   (yyval.str) = NewStringf("::%s",(yyvsp[(2) - (2)].str));
               }
    break;

  case 437:
#line 6074 "parser.y"
    {
		 (yyval.str) = NewStringf("::~%s",(yyvsp[(2) - (2)].str));
               }
    break;

  case 438:
#line 6080 "parser.y"
    {
                  (yyval.str) = NewStringf("%s%s",(yyvsp[(1) - (2)].id),(yyvsp[(2) - (2)].id));
		  /*		  if (Len($2)) {
		    scanner_last_id(1);
		    } */
              }
    break;

  case 439:
#line 6089 "parser.y"
    { 
                  (yyval.str) = 0;
		  if (!(yyval.str)) (yyval.str) = NewStringf("%s%s", (yyvsp[(1) - (2)].id),(yyvsp[(2) - (2)].str));
      	          Delete((yyvsp[(2) - (2)].str));
               }
    break;

  case 440:
#line 6094 "parser.y"
    { 
		 (yyval.str) = NewStringf("::%s%s",(yyvsp[(3) - (4)].id),(yyvsp[(4) - (4)].str));
                 Delete((yyvsp[(4) - (4)].str));
               }
    break;

  case 441:
#line 6098 "parser.y"
    {
		 (yyval.str) = NewString((yyvsp[(1) - (1)].id));
   	       }
    break;

  case 442:
#line 6101 "parser.y"
    {
		 (yyval.str) = NewStringf("::%s",(yyvsp[(3) - (3)].id));
               }
    break;

  case 443:
#line 6104 "parser.y"
    {
                 (yyval.str) = NewString((yyvsp[(1) - (1)].str));
	       }
    break;

  case 444:
#line 6107 "parser.y"
    {
                 (yyval.str) = NewStringf("::%s",(yyvsp[(3) - (3)].str));
               }
    break;

  case 445:
#line 6112 "parser.y"
    {
                   (yyval.str) = NewStringf("::%s%s",(yyvsp[(2) - (3)].id),(yyvsp[(3) - (3)].str));
		   Delete((yyvsp[(3) - (3)].str));
               }
    break;

  case 446:
#line 6116 "parser.y"
    {
                   (yyval.str) = NewStringf("::%s",(yyvsp[(2) - (2)].id));
               }
    break;

  case 447:
#line 6119 "parser.y"
    {
                   (yyval.str) = NewStringf("::%s",(yyvsp[(2) - (2)].str));
               }
    break;

  case 448:
#line 6122 "parser.y"
    {
		 (yyval.str) = NewStringf("::~%s",(yyvsp[(2) - (2)].id));
               }
    break;

  case 449:
#line 6128 "parser.y"
    { 
                   (yyval.id) = (char *) malloc(strlen((yyvsp[(1) - (2)].id))+strlen((yyvsp[(2) - (2)].id))+1);
                   strcpy((yyval.id),(yyvsp[(1) - (2)].id));
                   strcat((yyval.id),(yyvsp[(2) - (2)].id));
               }
    break;

  case 450:
#line 6133 "parser.y"
    { (yyval.id) = (yyvsp[(1) - (1)].id);}
    break;

  case 451:
#line 6136 "parser.y"
    {
		 (yyval.str) = NewString((yyvsp[(1) - (1)].id));
               }
    break;

  case 452:
#line 6139 "parser.y"
    {
                  skip_balanced('{','}');
		  (yyval.str) = NewString(scanner_ccode);
               }
    break;

  case 453:
#line 6143 "parser.y"
    {
		 (yyval.str) = (yyvsp[(1) - (1)].str);
              }
    break;

  case 454:
#line 6148 "parser.y"
    {
                  Hash *n;
                  (yyval.node) = NewHash();
                  n = (yyvsp[(2) - (3)].node);
                  while(n) {
                     String *name, *value;
                     name = Getattr(n,"name");
                     value = Getattr(n,"value");
		     if (!value) value = (String *) "1";
                     Setattr((yyval.node),name, value);
		     n = nextSibling(n);
		  }
               }
    break;

  case 455:
#line 6161 "parser.y"
    { (yyval.node) = 0; }
    break;

  case 456:
#line 6165 "parser.y"
    {
		 (yyval.node) = NewHash();
		 Setattr((yyval.node),"name",(yyvsp[(1) - (3)].id));
		 Setattr((yyval.node),"value",(yyvsp[(3) - (3)].id));
               }
    break;

  case 457:
#line 6170 "parser.y"
    {
		 (yyval.node) = NewHash();
		 Setattr((yyval.node),"name",(yyvsp[(1) - (5)].id));
		 Setattr((yyval.node),"value",(yyvsp[(3) - (5)].id));
		 set_nextSibling((yyval.node),(yyvsp[(5) - (5)].node));
               }
    break;

  case 458:
#line 6176 "parser.y"
    {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"name",(yyvsp[(1) - (1)].id));
	       }
    break;

  case 459:
#line 6180 "parser.y"
    {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"name",(yyvsp[(1) - (3)].id));
                 set_nextSibling((yyval.node),(yyvsp[(3) - (3)].node));
               }
    break;

  case 460:
#line 6185 "parser.y"
    {
                 (yyval.node) = (yyvsp[(3) - (3)].node);
		 Setattr((yyval.node),"name",(yyvsp[(1) - (3)].id));
               }
    break;

  case 461:
#line 6189 "parser.y"
    {
                 (yyval.node) = (yyvsp[(3) - (5)].node);
		 Setattr((yyval.node),"name",(yyvsp[(1) - (5)].id));
		 set_nextSibling((yyval.node),(yyvsp[(5) - (5)].node));
               }
    break;

  case 462:
#line 6196 "parser.y"
    {
		 (yyval.id) = (yyvsp[(1) - (1)].id);
               }
    break;

  case 463:
#line 6199 "parser.y"
    {
                 (yyval.id) = Char((yyvsp[(1) - (1)].dtype).val);
               }
    break;


/* Line 1267 of yacc.c.  */
#line 10325 "y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 6206 "parser.y"


SwigType *Swig_cparse_type(String *s) {
   String *ns;
   ns = NewStringf("%s;",s);
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSETYPE);
   yyparse();
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   return top;
}


Parm *Swig_cparse_parm(String *s) {
   String *ns;
   ns = NewStringf("%s;",s);
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSEPARM);
   yyparse();
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   Delete(ns);
   return top;
}


ParmList *Swig_cparse_parms(String *s, Node *file_line_node) {
   String *ns;
   char *cs = Char(s);
   if (cs && cs[0] != '(') {
     ns = NewStringf("(%s);",s);
   } else {
     ns = NewStringf("%s;",s);
   }
   Setfile(ns, Getfile(file_line_node));
   Setline(ns, Getline(file_line_node));
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSEPARMS);
   yyparse();
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   return top;
}


