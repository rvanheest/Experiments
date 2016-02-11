package experiments.fpPatterns.builderPattern;

import java.util.function.Consumer;

public class BuilderMailer3 {

	public static void main(String[] args) {
		// can I reuse the mailer here? - There is no mailer anymore...
		Mailer3.send(mailer -> mailer.from("richard.v.heest@gmail.com")
				.to("richardvanheest@solcon.nl")
				.subject("Your code works perfect!")
				.body("..."));
	}
}

class Mailer3 {

	public static void print(String msg) {
		System.out.println(msg);
	}

	private Mailer3() {
	}

	public Mailer3 from(String adres) {
		print("from");
		return this;
	}

	public Mailer3 to(String adres) {
		print("to");
		return this;
	}

	public Mailer3 subject(String line) {
		print("subject");
		return this;
	}

	public Mailer3 body(String msg) {
		print("body");
		return this;
	}

	public static void send(Consumer<Mailer3> block) {
		Mailer3 mailer = new Mailer3();
		block.accept(mailer);
		print("sending...");
	}
}
