
public final class Console {

    public static final Console $INSTANCE = new Console();

    public void print(String s){
        Rattlesnake$runtime.assertConsoleAllowed();
        System.out.print(s);
    }

    public String readLine(){
        Rattlesnake$runtime.assertConsoleAllowed();
        return System.console().readLine();
    }

}
