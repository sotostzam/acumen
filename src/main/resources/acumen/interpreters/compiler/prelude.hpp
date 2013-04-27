#include <cstdlib>
#include <cmath>
#include <cstdio>
#include <cstring>

using namespace std;

enum StepType {DISCRETE, CONTINUOUS};
enum KillMe {KEEP_ME, KILL_ME};

class AcumenObject {
public:
  const char * className_0;
  AcumenObject * parent;
  AcumenObject * children;
  AcumenObject * * children_ip; // ip = insertion point, children_ip always non-null, *children_ip always null
  AcumenObject * * back_ptr; // back_ptr always valid unless root, *back_ptr == this
  AcumenObject * next;
  virtual KillMe discrete_step(bool & somethingChanged) = 0;
  virtual void continuous_step(double stepSize) = 0;
  virtual void dump_header() = 0;
  virtual void dump_state() = 0;
  AcumenObject(const char * cn) 
    : className_0(cn), parent(), children(), children_ip(&children), back_ptr(), next() {}
  void add_child(AcumenObject * child) {
    child->parent = this;
    *children_ip = child;
    child->back_ptr = children_ip;
    children_ip = &child->next;
  }
  virtual ~AcumenObject();
};

