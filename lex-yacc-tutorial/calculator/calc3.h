typedef enum
  { typeCon
  , typeId
  , typeOpr
  } nodeEnum;

// constants
typedef struct {
  // value of constant
  int value;
} conNodeType;

// identifiers
typedef struct {
  // sym array index
  int i;
} idNodeType;

// operators
typedef struct {
  // operator
  int oper;
  // num of operands
  int nops;
  // operands
  struct nodeTypeTag **op;
} oprNodeType;

typedef struct nodeTypeTag {
  // type of node
  nodeEnum type;
  union {
    // constants
    conNodeType con;
    // identifiers
    idNodeType id;
    // operators
    oprNodeType opr;
  }
} nodeType;

extern int sym[26];
