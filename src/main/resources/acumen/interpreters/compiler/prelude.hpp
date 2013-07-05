#include <cstdlib>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <vector>
#include <string>
#include <sstream>

using namespace std;

enum StepType {DISCRETE, CONTINUOUS};
enum KillMe {KEEP_ME, KILL_ME};

class ObjectId {
public:
  vector<unsigned> data;
  void set(const ObjectId & prefix, unsigned id) {
    //data.reserve(prefix.data.size() + 1);
    data = prefix.data;
    data.push_back(id);
  }
  // FIXME: Using stringstream (and a lesser extent string) is a major
  // overkill
  string str_;
  const char * to_str() {
    if (!str_.empty()) return str_.c_str();
    stringstream res;
    res << "#";
    unsigned i = 0;
    while (i != data.size()) {
      res << data[i];
      ++i;
      if (i != data.size())
        res << ".";
    }
    str_ = res.str();
    return str_.c_str();
  }
};

class AcumenObject {
public:
  ObjectId id;
  unsigned next_child;
  const char * className_0;
  AcumenObject * parent;
  AcumenObject * children;
  AcumenObject * * children_ip; // ip = insertion point, children_ip always non-null, *children_ip always null
  AcumenObject * * back_ptr; // back_ptr always valid unless root, *back_ptr == this
  AcumenObject * next;
  virtual KillMe discrete_step(bool & somethingChanged) = 0;
  virtual void continuous_step(double stepSize) = 0;
  virtual void dump_header() = 0;
  virtual void dump_state_line() = 0;
  virtual void dump_state() = 0;
  AcumenObject(const char * cn, AcumenObject * p) 
    : next_child(0), className_0(cn), parent(p), children(), children_ip(&children), back_ptr(), next() 
  {
    if (p) {
      p->add_child(this);
      id.set(p->id, p->next_child);
      p->next_child++;
    }
  }
  void add_child(AcumenObject * child) {
    child->parent = this;
    *children_ip = child;
    child->back_ptr = children_ip;
    children_ip = &child->next;
  }
  virtual ~AcumenObject();
};

