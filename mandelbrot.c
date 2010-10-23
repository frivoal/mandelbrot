#include <stdio.h>
#include <stdbool.h>

bool escape(double x, double y, int limit, int * rank)
{
	double z_x = 0,
	       z_y = 0,
	       z_x2 = 0,
	       z_y2 = 0;
	int i = 0;

	do {
		if (i == limit)
			return false;
		i++;
		z_x2 = z_x * z_x;
		z_y2 = z_y * z_y;
		z_x = z_x2 - z_y2 + x;
		z_y = 2 * z_x *z_y + y;

	} while (z_x2 + z_y2 <= 4);
	if (rank)
		*rank = i;
	return true;
}

struct converter {
	double x_offset, y_offset;
	double x_scale, y_scale;
};

void init_converter( struct converter *c, unsigned int w, unsigned int h, double left, double right, double bottom, double top)
{
	double l, r, b, t;
	if ((double)w / (double)h < (right - left) / (top - bottom)) {
		l = left;
		r = right;
		double y_center = (top + bottom) / 2;
		double y_range = (right - left) * h / w / 2;
		b = y_center - y_range;
		t = y_center + y_range;
	} else {
		double x_center = (right + left) / 2;
		double x_range = (top - bottom) * w / h / 2;
		l = x_center - x_range;
		r = x_center + x_range;
		b = bottom;
		t = top;
	}
	c->x_scale = (r - l) / w;
	c->y_scale = (t - b) / h;
	c->x_offset = l + c->x_scale / 2;
	c->y_offset = b + (h - 0.5) * c->y_scale;
}

double conv_x( struct converter *c, unsigned int x)
{
	return x * c->x_scale + c->x_offset;
}

double conv_y( struct converter *c, unsigned int y)
{
	 return - (double)y  * c->y_scale  + c->y_offset;
}


int main()
{
	struct converter c;
	init_converter( &c, 1024, 768, -2.5, 1, -1.5, 1.5);

	printf("start\n");
	for (unsigned int y = 0; y < 1024; y++)
	{
		for(unsigned int x = 0; x < 768; x++)
		{
			escape(conv_x(&c, x), conv_y(&c, y), 1000, NULL);
		}
	}

	printf("end\n");
	return 0;
}
