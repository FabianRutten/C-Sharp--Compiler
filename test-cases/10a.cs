class Hello
{
	void main()
	{
		print(fac(1));
		print(fac(2));
		print(fac(10));
	}

	int fac(int x)
	{
		if (x == 1)
		{
			return 1;
		}
		else
		{
			return x * fac(x - 1);
		}
	}
}
