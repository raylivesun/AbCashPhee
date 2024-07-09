import 'dart:io';

void main() {
  Pipe.createSync();
  Pipe.create();
  return ;
}

mixin pipe {
  void createSync() {
    Pipe.createSync();
  }

  void create() {
    Pipe.create();
  }
}
