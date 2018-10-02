public class BaseConvert {
    public static String convert(String s, int fromBase, int toBase) {
        return Integer.toString(Integer.parseInt(s, fromBase), toBase);
    }
    public static void main(String[] args) {
        System.out.println(convert("FF", 16, 10));
        System.out.println(convert("FF", 16, 2));
    }
}
