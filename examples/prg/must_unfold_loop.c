// The assertion is violated only after several loop iterations.

int must_unfold_loop ()
{
	int x, y, z;

	x = 0;
	z = 20;
	y = 0;
	for (x = 0; x < 20; x++)
	{
		if (*)
			y++;
		assert (x*y < z);
	}
}
