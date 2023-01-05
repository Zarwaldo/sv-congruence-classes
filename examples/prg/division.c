// We should use the division somewhere.

int division ()
{
	int x, y;

	x = 30;
	y = 10;
	while (x > 1)
	{
		x = x/y;
		y++;
		assert(x < y);
	}
}
