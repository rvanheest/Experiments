package experiments.java.fpPatterns.decoratorPattern;

import java.awt.Color;
import java.util.function.Function;
import java.util.stream.Stream;

@SuppressWarnings("unchecked")
public class DecoratorCamera {

	public static void main(String[] args) {
		printSnap(new Camera());
		printSnap(new Camera(Color::brighter));
		printSnap(new Camera(Color::darker));
		printSnap(new Camera(Color::brighter, Color::darker));
	}

	public static void printSnap(Camera camera) {
		System.out.println(camera.snap(new Color(125, 125, 125)));
	}
}

@SuppressWarnings("unchecked")
class Camera {

	private Function<Color, Color> filter;

	public Camera(Function<Color, Color>... filters) {
		this.setFilters(filters);
	}

	public void setFilters(Function<Color, Color>... filters) {
		this.filter = Stream.of(filters).reduce(Function.identity(), Function::andThen);
	}

	public Color snap(Color input) {
		return this.filter.apply(input);
	}
}
