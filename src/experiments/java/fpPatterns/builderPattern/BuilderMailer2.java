package experiments.java.fpPatterns.builderPattern;

public class BuilderMailer2 {

	public static void main(String[] args) {
		// remove noise - cascade method pattern or builder pattern
		new Mailer2()
    		.from("richard.v.heest@gmail.com")
    		.to("richardvanheest@solcon.nl")
    		.subject("Your code works perfect!")
    		.body("...")
    		.send();
	}
}

class Mailer2 {

	public static void print(String msg) {
		System.out.println(msg);
	}

	public Mailer2 from(String adres) {
		print("from");
		return this;
	}

	public Mailer2 to(String adres) {
		print("to");
		return this;
	}

	public Mailer2 subject(String line) {
		print("subject");
		return this;
	}

	public Mailer2 body(String msg) {
		print("body");
		return this;
	}

	public void send() {
		print("sending...");
	}
}
