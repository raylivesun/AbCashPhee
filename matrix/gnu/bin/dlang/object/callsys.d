module matrix.gnu.bin.dlang.object.callsys;

class MyClass
{
    interface MyInterface
    {
        void call(MyInterface my)(MyClass my) {
            my.myMethod();            
        }

        void myMethod();    
    }
}