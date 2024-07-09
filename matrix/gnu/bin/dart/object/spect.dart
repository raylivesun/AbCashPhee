dynamic specification = Finalizer;

mixin callback {
  void dispose() {
    specification.dispose();
  }
  void finalize() {
    specification.finalize();
  }
  void initState() {
    specification.initState();
  }
}

void main() {
  test('Finalizer', () {
    final c = Object();
    c.dispose();
    c.finalize();
    c.initState();
  });
}

callback Object() => Object();

void test(String s, Null Function() param1) {
  print(s);
  param1();
}
