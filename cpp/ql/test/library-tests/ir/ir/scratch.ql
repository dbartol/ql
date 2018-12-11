import cpp

string getQName(Function f) {
  result = f.getQualifiedName() or
  not exists(f.getQualifiedName()) and result = "??"
}

from Function func
select func, getQName(func), func.getDeclaringType().getUnspecifiedType()