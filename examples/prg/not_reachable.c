// Look well, it is actually impossible to reach the assert.
// Still, here verification fails.

int not_rechable ()
{
	int x, y, z;

	x = 0;
	while (x < 10)
	{
		for (y = x; y < 0; y++)
		{
			for (z = 0; z < 10; z++)
			{
				x = x+1;
			}
			assert (0 == 1);
		}
	}
}
