module dlang.list.files;

class FileSystem
{
     interface MyInterface
     {
        void open(FileSystem fileSystem)(ref string) {
            return file(fileSystem);
        }
     }   
}