
AcumenObject::~AcumenObject() {
  if (children) {
    // splice in children
    *back_ptr = children;
    children->back_ptr = back_ptr;
    *children_ip = next;
    if (next) next->back_ptr = children_ip;
    else if (parent) parent->children_ip = children_ip;
  } else {
    *back_ptr = next;
    if (next) next->back_ptr = back_ptr;
    else if (parent) parent->children_ip = back_ptr;
  }
}

static void dump_header(AcumenObject * root) {
  root->dump_header();
  for (AcumenObject * node = root->children; node; node = node->next)
    dump_header(node);
}

static void dump_state_line(AcumenObject * root) {
  root->dump_state_line();
  for (AcumenObject * node = root->children; node; node = node->next)
    dump_state_line(node);
}

static void dump_state(AcumenObject * root) {
  root->dump_state();
  for (AcumenObject * node = root->children; node; node = node->next)
    dump_state(node);
}

static void discrete_step(AcumenObject * root, bool & somethingChanged) {
  KillMe res = root->discrete_step(somethingChanged);
  for (AcumenObject * node = root->children; node; node = node->next)
    discrete_step(node, somethingChanged);
  if (res == KILL_ME) {
    delete root;
    somethingChanged = true;
  }
}

static void continuous_step(AcumenObject * root, double stepSize) {
  root->continuous_step(stepSize);
  for (AcumenObject * node = root->children; node; node = node->next)
    continuous_step(node, stepSize);
}

void main_loop(AcumenObject * root, Simulator * simulator, bool last) {
  if (!last) {
    dump_header(root);
    printf("\n");
  }
  while (simulator->time_0 < simulator->endTime_0) {
    simulator->stepType_0 = DISCRETE;
    bool somethingChanged;
    do {
      somethingChanged = false;
      if (!last) {
        dump_state_line(root);
        printf("\n");
      }
      discrete_step(root, somethingChanged);
    } while (somethingChanged);
    simulator->stepType_0 = CONTINUOUS;
    continuous_step(root, simulator->timeStep_0);
    simulator->time_0 += simulator->timeStep_0;
    if (!last) {
      dump_state_line(root);
      printf("\n");
    }
  }
  if (last) {
    dump_state(root);
  }
}

int main(int argc, const char* argv[]) {
  bool last = false;
  if (argc == 2 && strcmp(argv[1], "last") == 0) {
    last = true;
  }
  Simulator * simulator = new Simulator(NULL);
  Main * main = new Main(NULL, simulator);
  main->add_child(simulator);
  main->id.data.push_back(0);
  simulator->id.set(main->id, main->next_child);
  main->next_child++;
  main_loop(main, simulator, last);
}
