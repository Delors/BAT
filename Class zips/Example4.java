
public class Example4 {
	
	public int myif(int x, int y){
		int z;
		if(x > y){
			z = x + y;
		}
		else {
			z = x - y;
		}
		return z;
	}
	
	public int myfor (int x, int y, int i, int max){
		int res = 0;
		for(int j = i; j < max; j++){
			res = res + x * y;
		}
		return res;
	}

}
