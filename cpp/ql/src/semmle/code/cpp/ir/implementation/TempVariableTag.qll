private import internal.TempVariableTagInternal

class TempVariableTag extends Language::TTempVariableTag {
  string toString() { result = Language::getTempVariableTagId(this) }
}
