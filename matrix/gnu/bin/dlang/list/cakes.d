module matrix.gnu.bin.dlang.list.cakes;

class CakeScript
{
    interface MyInterface
    {
        void script(CakeScript script)(ref auto language) {
             script.script(language);
        }
    }    
}
