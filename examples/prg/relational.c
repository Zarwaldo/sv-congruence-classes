// Example that requires a relational analysis.

int relational ()
{
	int x, y, z;

	x = 1;
	for (y = 0 ; y < z ; y++)
	{
		if (*)
			x = x + 5;
		else
			x = x + 3;
	}
	assert (x > 2*z);
}
