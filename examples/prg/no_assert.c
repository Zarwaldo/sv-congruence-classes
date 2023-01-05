// There is no assert here. Maybe backward checking would work?

int no_assert ()
{
	int x, y, z;

	for (x = 0; x < 20; x++)
	{
		for (y = 0; y < x*x; y++)
		{
			if (*)
				z++;
			else
				z--;
		}
	}
}
