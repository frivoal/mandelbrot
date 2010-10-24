#include <stdio.h>
#include <stdbool.h>
#include <gtk/gtk.h>

unsigned int escape(double x, double y, int limit)
{
	double z_x = 0;
	double z_y = 0;
	double z_x2;
	double z_y2;
	int i = 0;

	do {
		if (i++ == limit)
			return 0;
		z_x2 = z_x * z_x;
		z_y2 = z_y * z_y;
		z_y = 2 * z_x *z_y + y;
		z_x = z_x2 - z_y2 + x;

	} while (z_x2 + z_y2 <= 4);
	return i;
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


void destroy(GtkWidget * widget, gpointer data)
{
	gtk_main_quit();
}

unsigned int index_clip(unsigned int i,unsigned int max)
{
	if (!i)
		return 0;
	else
		return (i - 1) % max + 1;
}

gboolean paint( GtkWidget * widget, GdkEventExpose * event, gpointer data )
{
	GdkGC *gcs[2];
	gcs[0] = gdk_gc_new( widget->window );
	gcs[1] = gdk_gc_new( widget->window );
	GdkColor color;
	color.red = 0;
	color.green = 0;
	color.blue = 0;
	gdk_gc_set_rgb_fg_color( gcs[0], &color );
	color.red = 0xFFFF;
	color.green = 0xFFFF;
	color.blue = 0xFFFF;
	gdk_gc_set_rgb_fg_color( gcs[1], &color );

	int w = widget->allocation.width;
	int h = widget->allocation.height;

	struct converter c;
	init_converter(&c, w, h, -2.5, 1, -1.5, 1.5);
	double *xs = g_malloc(sizeof(double)*w);
	double *ys = g_malloc(sizeof(double)*h);
	for (int x = 0; x < w; x++)
		xs[x] = conv_x(&c, x);
	for (int y = 0; y < h; y++)
		ys[y] = conv_y(&c, y);
	for (int y = 0; y < h; y++)
	{
		for (int x = 0; x < w; x++)
		{
			gdk_draw_point(widget->window, gcs[index_clip(escape(xs[x], ys[y], 30),1)], x, y);
		}
	}
	g_free(xs);
	g_free(ys);

	g_object_unref(gcs[0]);
	g_object_unref(gcs[1]);
	return TRUE;
}

int main(int argc, char *argv[])
{
	gtk_init(&argc, &argv);

	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_resize(GTK_WINDOW(window), 300, 300);
	g_signal_connect( G_OBJECT(window), "destroy", G_CALLBACK(destroy), NULL);

	GtkWidget *draw_area = gtk_drawing_area_new();
	gtk_container_add( GTK_CONTAINER(window), GTK_WIDGET(draw_area) );
	g_signal_connect( draw_area, "expose-event", G_CALLBACK(paint), NULL );

	gtk_widget_show(draw_area);
	gtk_widget_show(window);

	gtk_main();

	return 0;
}
