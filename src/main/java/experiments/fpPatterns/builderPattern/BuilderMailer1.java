package experiments.fpPatterns.builderPattern;

public class BuilderMailer1 {

	public static void main(String[] args) {
		Mailer1 mailer = new Mailer1();
		mailer.from("richard.v.heest@gmail.com");
		mailer.to("richardvanheest@solcon.nl");
		mailer.subject("Your code works perfect!");
		mailer.body("...");
		mailer.send();
		
		// can I reuse the mailer here?
	}
}

class Mailer1 {

	public static void print(String msg) {
		System.out.println(msg);
	}

	public void from(String adres) {
		print("from");
	}

	public void to(String adres) {
		print("to");
	}

	public void subject(String line) {
		print("subject");
	}

	public void body(String msg) {
		print("body");
	}

	public void send() {
		print("sending...");
	}
}
