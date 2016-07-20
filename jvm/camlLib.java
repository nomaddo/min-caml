class CamlLib
{
    public static int min_caml_print_int(int i)
    {
        System.out.println(i);
        return 0;
    }

    public static int min_caml_truncate(double d)
    {
        return (int)d;
    }

    public static double[] min_caml_create_array(int i, double d)
    {
        double[] arr = new double[i];
        for(int ind = 0; ind<i; ind++)
            arr[ind] = d;
        return arr;
    }

    public static int[] min_caml_create_array(int i, int d)
    {
        int[] arr = new int[i];
        for(int ind = 0; ind<i; ind++)
            arr[ind] = d;
        return arr;
    }

    public static Object[] min_caml_create_array(int i, Object o)
    {
        Object[] arr = new Object[i];
        for(int ind = 0; ind < i; ind++)
            arr[ind] = o;
        return arr;
    }

}
