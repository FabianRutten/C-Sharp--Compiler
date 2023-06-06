class Hello
{
    //int a;

    void main()
    {
        int a;
        a = 37;

        {
            int a;
            a = 35;
            print (a);
        }

        print (a);

        //a = 5;
        //b = 7;
        //int c;
        //c = 9;
        //d = 0;
        //print(a + d);
        //p(c);
        //print(a, b, c);
        for (int i, i = 0; i < 5; i = i + 1) {
            print(i);
        }

        //print (i);


        //if(true)
        //{
        //    print(f(5, 7)); // 5 7 12 12
        //} 
        //else
        //{
        //    print(f(5,7+1)); // 5 8 13 13
        //} 
        //print(f(5, 7)); // 5 7 12 12
        //print(b() || b()); // als het lazy is wordt 98 (b) niet geprint
    }

    //int b;

    //int f(int x, int y)
    //{
    //    int retval;
    //    print(x, y);
    //    retval = x + y;
    //    print(retval);
    //    return retval;
    //}

    //bool b()
    //{
    //    print('b');
    //    return true;
    //}

    //void p(int c)
    //{
    //    int a;
    //    a = 0;
    //    print(a, b, c);
    //}

    //int g;
    
    //void main()
    //{
    //    //int b;
    //    //b = 1;
    //    //print(42);
    //    //print(6 * 7, 42 + 1);
    //    //print(1 == 1, 0 == 1);
    //    //print(true && true, false || true, false && true, false ^ true, true ^ true, 'a' + 1, '*');
    //    print(2 + 2 * 2); // 6 (niet 8)
    //    print(2 * 2 + 2); // 6 (niet 8)
    //    int a;
    //    int b;
    //    b = 0;
    //    a = b = 1;
    //    b = 2;
    //    print(a, b);
    //    //b = square(5);
    //}
    
    //int square( int x )
    //{
    //    int y;
    //    y = x*x;
    //    return y;   
    //}
    //
    //int abs(int x)
    //{
    //	
    //    if (x<0)
    //        x = 0-x;
    //    return x;
    //}
    //
    //int fac(int x)
    //{
    //    int r; int t;
    //    t=1; r=1;
    //    while (t<=x)
    //    {
    //        r = r*t;
    //        t = t+1;
    //    }
    //    return r;
    //}
}
