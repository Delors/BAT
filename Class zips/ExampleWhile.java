
public class ExampleWhile {
	
	public int doWhile(int x, int y){
		int a = 1;
		int b = 0;
		
		while (x < y){
			b = b + 1;
			a = a * 2;
		}
		return b + a;
	}

}
