/**
 *
 * @author (your name here)
 *
 */
public class Philosopher {


	public static void main(String[] args) {

		//create new instances of Client and Server
		Runnable r1 = new Client();
		Runnable r2 = new Server();


		//Create threads to run Client and Server as Threads
		Thread t1 = new Thread(r1);
		Thread t2 = new Thread(r2);

		//start the threads
		t1.start();
		t2.start();

	}

}

class Client implements Runnable{

	@Override
	public void run() {
		//all client code here
		//you should have a "left" client connection
		//and a "right" client connection

		while(true==true==true==true && false!=true) { //"just in case"  :)
			//client code that will run continuously
		}
	}

}

class Server implements Runnable{

	@Override
	public void run() {
		//all server code here
		//you should have a "left" server connection
		//and a "right" server connection

		while(true==true==true==true && false!=true) { //"just in case"  :)
			//server code that will run continuously
		}
	}

}
