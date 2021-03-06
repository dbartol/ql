var stringVar: string = "";
var numberVar: number = 0;
var booleanVar: boolean = false;
var nullVar: null = null;
var undefinedVar: undefined
var anyVar: any;
var voidVar: void;
var neverVar: never;
var literalString: "foo"
var literalNumber: 45
var literalTrue: true
var literalFalse: false

interface Interface {
  numberField: number;
  stringField: string;
  interfaceField: Interface;
  thisField: this;
  
  returnNumberMethod(): number;
  returnVoidMethod(): void;
  returnNullMethod(): null;
  returnThisMethod(): this;
  takeNumberMethod(numberMethodParam: number);
  takeInterfaceMethod(interfaceMethodParam: Interface);
}

var interfaceVar: Interface;

function returnNumberFunction(): number { return 0; }
function returnVoidFunction(): void { return 0; }
function returnNullFunction(): null { return 0; }
function takeNumberFunction(numberFunctionParam: number) {}
function takeInterfaceFunction(interaceFunctionParam: Interface) {}

function typesAndDefaults(param1: number = 1, param2: string = '2') {}

var arrayType: string[]
var arrayType2: string[][]
var arrayType3: string[][][]
var unionType: string|number|boolean;
var indexedType: Interface['numberField'];
var intersectionType: string&number&boolean;
var parenthesizedType: (string);
var parenthesizedType2: ((string));
var parenthesizedType3: (((string)));
var complexType: (string | number) & (boolean | string);
var tupleType: [number, string, boolean]
var keyofType: keyof Interface;

interface Generic<T> {}
interface ManyTypeArgs<S, T, U> {}
namespace N {
  export interface I {}
  export interface InnerGeneric1<T> {}
  export namespace M {
    export interface J {}
    export interface InnerGeneric2<T> {}
  }
  export var x = 5;
}

var qualifiedVar: N.I;
var qualifiedVar2: N.M.I;
var genericVar1: Generic<number>
var genericVar2: N.InnerGeneric1<number>
var genericVar3: N.M.InnerGeneric2<number>
var genericVar4: Generic<N.InnerGeneric1<number>>
var genericVar5: ManyTypeArgs<number, string, boolean>
var typeofVar1: typeof N
var typeofVar2: typeof N.x
var typeofVar3: typeof qualifiedVar

interface Node {
  isThisLeaf(): this is Leaf;
  isThatLeaf(that): that is Leaf;
}
interface Leaf {}

function complexIsType(x: number): x is Generic<Leaf[]> { return false; }
function obviousIsType(x: string[]): x is typeof x { return true }

var interfaceVar: { x: number; y: number; }
var emptyInterfaceVar: {}
var interfaceVarMethod: { foo(): number; }

var functionType1: () => number;
var functionType2: (param: string) => number;
var functionType3: <T>(param: T) => T;
var constructorType1: new () => Object;
var constructorType2: new (param: string) => Object;
var constructorType3: new <T>(param: T) => Object;

function f1<S>(x: S): S { return x; }
function f2<S,T>(x: S, y: T): [S,T] { return [x,y]; }
function f3<S extends number>(x: S): S { return x; }

class C1<S> {}
class C2<S,T> {}
class C3<S extends number> {}

var call1 = f1<string>("foo");
var call2 = f2<string, number>("foo", 5);
var call3 = f3<number>(5);

var new1 = new C1<string>();
var new2 = new C2<string, number>();
var new3 = new C3<number>();

var asCast = 5 as number;
var prefixCast = <string> 'foo';

var mappedType1: { [K in keyof Node]: number }
var mappedType2: { [K in "foo"]: number }

function hasThisParam(this: void, x: number, y: string) {}

interface InterfaceWithThisParam {
  hasThisParam(this: InterfaceWithThisParam);
}

var importedType: import("type");
var importedTypeGeneric: import("type")<string>;
var importedQualifiedType: import("namespace").Foo;
var importedQualifiedTypeGeneric: import("namespace").Foo<string>;
var importedTypeof: typeof import("value");
var importedQualifiedTypeof: typeof import("value").x;
var importedQualifiedTypeWhitespace: import(
  'awkard-namespace'
  )
  .bar;

let tupleWithOptionalElement: [number, string, number?];
let emptyTuple: [];
let tupleWithRestElement: [number, ...string[]];
let tupleWithOptionalAndRestElements: [number, string?, ...number[]];
let unknownType: unknown;

let taggedTemplateLiteralTypeArg1 = someTag<number>`Hello`;
let taggedTemplateLiteralTypeArg2 = someTag<number, string>`Hello`;
