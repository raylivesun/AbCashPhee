
dynamic bool = false;

class Dynamic<T> {
  late Set DomName;
  late T value;
  Dynamic(this.DomName, this.value);
}

void main(List<String> args) {
  var domName = args[0];
  var value = args[1];
  var dynamic = new Dynamic(domName as Set, value);
  print(dynamic);
}
