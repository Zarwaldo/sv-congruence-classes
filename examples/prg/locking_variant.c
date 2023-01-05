// Example inspired from Lazy Abstraction by Henzinger, Jhala, Majumdar,
// and Sutre. POPL 2002.

int locking ()
{
	int L;
	int old, new;

	L = 0;
	new = old + 1;
	while (new != old)
	{
		// Lock
		assert (L == 0);
		L = 1;
		old = new;
		if (*)
		{
			// Unlock
			assert (L == 1);
			L = 0;
			new++;
		}
	}
}
