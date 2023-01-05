// Assertion is violated, but you need depth around 120 to see it.

int long_to_violate ()
{
	int x, y;

	x = 0;
	while (x < 20)
	{
		if (y < 0)
			y = y+x;
		else
			y = y-x;
		if (*)
			y = y+200;
		x++;
	}
	assert (y > 0);
}
