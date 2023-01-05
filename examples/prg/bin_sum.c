// Binary and unary sums.  The assertion checks that the unary sum is valid.

int bin_sum ()
{
	int x, y, zbin, zun;

	zbin = x + y;

	zun = 0;

	while (x > 0) {
		x = x - 1;
		zun = zun + 1;
	}

	while (x < 0) {
		x = x + 1;
		zun = zun - 1;
	}

	while (y > 0) {
		y = y - 1;
		zun = zun + 1;
	}

	while (y < 0) {
		y = y + 1;
		zun = zun - 1;
	}

	assert (zbin == zun);
}
