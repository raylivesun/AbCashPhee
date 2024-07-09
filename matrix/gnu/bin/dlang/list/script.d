module matrix.gnu.bin.dlang.list.script;

class CakeScript
{
    interface MyInterface
    {
        void script(CakeScript script)(ref auto language) {
             script.script(language);
        }
    }    
}
