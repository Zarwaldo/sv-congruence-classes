// Look well, it is actually impossible to reach the assert.

int not_rechable ()
{
	int x, y;

	x = 0;
	while (x < 10)
	{
		for (y = x; y < 0; y++)
		{
			assert (0 == 1);
		}
		x++;
	}
}
