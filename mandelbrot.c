#include <stdio.h>
#include <stdbool.h>
#include <gtk/gtk.h>

int escape(double x, double y, int limit)
{
	double z_x = x;
	double z_y = y;
	double z_x2;
	double z_y2;
	int i = -1;

	do {
		if (i++ == limit)
			return -1;
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

struct color {
	unsigned char r;
	unsigned char g;
	unsigned char b;
};

struct paint_env {
	unsigned int size;
	struct color *c;
	double l, r, b, t;
	int iter;
	int w, h;
	int *set;
};

unsigned int interpolate_colors(unsigned int split, struct color *src, unsigned int size, struct color *dst)
{
	if (size < 2)
		return 0;
	if (dst) {
		struct color *res = dst;
		for (unsigned int i = 0; i < size - 1; i++) {
			*res++ = src[i];
			double r_step = (src[i + 1].r - src[i].r) / ((double)split + 1);
			double g_step = (src[i + 1].g - src[i].g) / ((double)split + 1);
			double b_step = (src[i + 1].b - src[i].b) / ((double)split + 1);
			for (unsigned int j = 1; j < split + 1; j++) {
				res->r = (unsigned char)(src[i].r + j * r_step);
				res->g = (unsigned char)(src[i].g + j * g_step);
				res->b = (unsigned char)(src[i].b + j * b_step);
				res++;
			}
		}
		*res++ = src[size - 1];
	}
	return (size - 1)* (split  + 1) + 1;
}

void init_gc_palette(struct color *src, unsigned int size, GdkGC **dst, GdkDrawable *d)
{
	GdkColor c;
	for( unsigned int i = 0; i < size; i++) {
		c.red = src[i].r << 8;
		c.green = src[i].g << 8;
		c.blue = src[i].b << 8;
		dst[i] = gdk_gc_new(d);
		gdk_gc_set_rgb_fg_color(dst[i], &c);
	}
}

void destroy_gc_palette(GdkGC **p, unsigned int size)
{
	for( unsigned int i = 0; i < size; i++)
		g_object_unref(p[i]);
}

void render(struct paint_env *p)
{
	double *xs = g_malloc(sizeof(double) * p->w);
	double *ys = g_malloc(sizeof(double) * p->h);

	struct converter c;
	init_converter(&c, p->w, p->h, p->l, p->r, p->b, p->t);
	for (int x = 0; x < p->w; x++)
		xs[x] = conv_x(&c, x);
	for (int y = 0; y < p->h; y++)
		ys[y] = conv_y(&c, y);

	for (int y = 0; y < p->h; y++)
	{
		for (int x = 0; x < p->w; x++)
		{
			p->set[y * p->w + x] = escape(xs[x], ys[y], p->iter);
		}
	}
	g_free(xs);
	g_free(ys);
}

gboolean paint( GtkWidget * widget, GdkEventExpose * event, gpointer data )
{
	struct paint_env *p = (struct paint_env*)data;

	if (p->set) {
		GdkGC **gc = g_malloc(sizeof(GdkGC*) * p->size);
		init_gc_palette(p->c, p->size, gc, widget->window);
		GdkGC **shifted_gc = gc + 1;
		int size = p->size -1;

		int w = MIN( widget->allocation.width, p->w);
		int h = MIN( widget->allocation.height, p->h);

		for (int y = 0; y < h; y++)
		{
			for (int x = 0; x < w; x++)
			{
				gdk_draw_point(widget->window, shifted_gc[p->set[y * p->w + x] % size], x, y);
			}
		}

		destroy_gc_palette(gc, 3);
	}

	return TRUE;
}

void set_palette( struct paint_env *p, struct color *c, unsigned int size,  unsigned int splits)
{
	p->size = 1 + interpolate_colors(splits, c, size, NULL);
	g_free(p->c);
	p->c = g_malloc(sizeof(struct color) * p->size);
	struct color black = {0, 0, 0};
	p->c[0] = black;
	interpolate_colors(splits, c, size, p->c+1);
}

void destroy(GtkWidget * widget, gpointer data)
{
	struct paint_env *p = (struct paint_env*)data;
	g_free( p->c );
	gtk_main_quit();
}

gboolean click( GtkWidget * widget, GdkEventButton * event, gpointer data )
{
	struct paint_env * p = (struct paint_env*)data;
	if (event->button == 3) {
		p->iter += 10;
		render(p);
		gdk_window_invalidate_rect(widget->window, NULL, FALSE);
	} else if (event->button == 1) {
		double x = event->x;
		double y = event->y;
		int w = widget->allocation.width;
		int h = widget->allocation.height;
		struct converter c;
		init_converter(&c, w, h, p->l, p->r, p->b, p->t);
		double center_x = p->l + (p->r - p->l) * x / w;
		double center_y = p->b + (p->t - p->b) * (h - y) / h;
		double zoomed_w = conv_x(&c, w) - conv_x(&c, 0);
		double zoomed_h = conv_y(&c, 0) - conv_y(&c, h);
		p->l = center_x - zoomed_w / 2.5;
		p->r = center_x + zoomed_w / 2.5;
		p->b = center_y - zoomed_h / 2.5;
		p->t = center_y + zoomed_h / 2.5;
		render(p);
		gdk_window_invalidate_rect(widget->window, NULL, FALSE);
	}
	return TRUE;
}

gboolean resize(GtkWidget *w, GdkEventConfigure *event, gpointer data)
{
	struct paint_env *p= (struct paint_env*)data;
	if (event->width != p->w || event->height != p->h)
	{
		g_free(p->set);
		p->w = event->width;
		p->h = event->height;
		p->set = g_malloc0(p->w * p->h * sizeof(int));
		render(p);
	}
	gdk_window_invalidate_rect(w->window, NULL, FALSE);

	return TRUE;
}
int main(int argc, char *argv[])
{
	gtk_init(&argc, &argv);

	struct paint_env p = {0, NULL, -2.5, 1, -1.5, 1.5, 30, 0, 0, NULL};
	struct color c[] = {{0, 0, 64}, {0, 255, 255}, {255, 128, 0}, {0, 0, 64}};
	set_palette( &p, c, sizeof(c)/sizeof(struct color), 15);

	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_resize(GTK_WINDOW(window), 300, 300);
	g_signal_connect( G_OBJECT(window), "destroy", G_CALLBACK(destroy), &p);

	GtkWidget *draw_area = gtk_drawing_area_new();
	gtk_container_add( GTK_CONTAINER(window), GTK_WIDGET(draw_area) );
	g_signal_connect( draw_area, "expose-event", G_CALLBACK(paint), &p );
	g_signal_connect( draw_area, "button-press-event", G_CALLBACK(click), &p );
	g_signal_connect( draw_area, "configure-event", G_CALLBACK(resize), &p );
	gtk_widget_add_events(draw_area, GDK_BUTTON_PRESS_MASK);

	gtk_widget_show(draw_area);
	gtk_widget_show(window);

	gtk_main();

	return 0;
}
