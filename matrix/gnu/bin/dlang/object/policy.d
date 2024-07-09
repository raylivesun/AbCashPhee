module matrix.gnu.bin.dlang.object.policy;

class MyClass
{
    interface MyInterface
    {
        void set(MyInterface value)(ref my) {
            my = value;
        }
        MyInterface get()(ref my) {
            return my;
        }
    }
} 