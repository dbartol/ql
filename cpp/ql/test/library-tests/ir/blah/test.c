typedef struct _GSList GSList;

struct _GSList
{
  void* data;
  GSList *next;
};

void foo(void) {
    GSList* p;
}