import 'dart:collection';

var cnn = Object();

class CNN<T> {
  late SetBase setBase;
  late MapBase mapBase;
  late ListBase listBase;      
}

void main() {
  var cnn = CNN();
  cnn.setBase = HashSet() as SetBase;
  cnn.mapBase = HashMap() as MapBase;
  cnn.listBase = ArrayList() as ListBase;
}

class ArrayList {
  List list = [];
  void add(var value) {
    list.add(value);
  }
  void remove(var value) {
    list.remove(value);
  }

}



