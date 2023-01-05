// Running example for the course.

int running ()
{
	int x, y;

	x = 1;
	if (y <= 10)
		y = 10;
	else
		while (x < y)
		{
			x = 2 * x;
			y--;
		}
	x = y+1;
	assert (x > 0);
}
