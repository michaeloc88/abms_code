import java.math.*;
import java.util.Random;
import java.io.*;

class coop implements Parameters
{
     public static void main(String[] args)
     {
	int nra=1000; // 100-1000
	int nrg=15; // 1 - 15
	double B=40; // 0-1
	double mr=0.05; // 0.005 - 0.1
	double pmut=0.01; // 0.005 - 0.1
	double[] pg = new double[nrg]; // public good
	double[] avgx=new double[nrg];
	double[] avgy=new double[nrg];
 	int[] gx = new int[nra]; // agent is in group ..
	double sumx=0;
	double sumy=0;
	double[] x = new double[nra];
	int nrobs[] = new int[nra];
	double draw=0;
	double[] px = new double[nra];
	int counter=0;
	double[] xx = new double[nra];
	int[] nrag = new int[nra];
	int[] gxx = new int[nra];
	double[] y = new double[nra];
	int number=0;
	double totx=0;
	double suma=0;
	int found=0;
	int agent=0;
	double maxg=0;
	double summaxg=0;
	double sumat=0;
	double summaxgt=0;
	double maxp=0;
	double pen=0;
	double cost=0.1;
	double suml=0;
	double sumyy=0;
	double sumyyt=0;
	double migr = 0;
	double summigr=0;
	double summigrt=0;
	Random rnd = new Random();
	int[] prevnrag = new int[nrg];
	int totr = 0;

	try
	{
		FileWriter fw = new FileWriter("test.txt");
		FileWriter fw2 = new FileWriter("test2.txt");
		FileWriter fw4 = new FileWriter("test4.txt");
		FileWriter fw5 = new FileWriter("test5.txt");
		mr=0.006;
		pmut=0.006;
		B=10;
		nra=1000;
		nrg=10;
		for (int j3=0; j3<nra; j3++)
		{
			nrobs[j3]=0;
		}
		for (int j1=2; j1<=2; j1++)
		{
			mr=0.003*j1;
		//	pmut=0.003*j1;
		//	nra=40*j1;
			totr=2000*(1000/nra);
			for (int j2=2; j2<=2; j2++)
			{
				pmut=0.003*j2;
			//	B=2*j2;
			//	nrg=j2;
				System.out.println(" " + nra + " " + nrg);
				totx=0;
				sumat=0;
				sumyyt=0;
				summaxgt=0;
				summigrt=0;
				for (int j3=1; j3<=1; j3++)
				{
					suma=0;	
					sumyy=0;
					summaxg=0;
					summigr=0;		
					for (int i3=0; i3<nra; i3++)
					{
						x[i3]=Math.random();
						gx[i3]=rnd.nextInt(nrg);
					}
					sumx=0;
					for (int i3=0; i3<nrg; i3++)
					{
						nrag[i3]=0;
					}
					for (int i3=0; i3<nra; i3++)
					{
						if (Math.random()<pmut)
						{
							x[i3]=Math.random();	
						}
						sumx=sumx+x[i3];	
						nrag[gx[i3]]=nrag[gx[i3]]+1;
					}
					for (int i1=0; i1<=totr; i1++)
					{
						// evaluate
						for (int i2=0; i2<nrg; i2++)
						{
							pg[i2]=0;
							avgx[i2]=0;
							avgy[i2]=0;
						}
						for (int i2=0; i2<nra; i2++)
						{
							pg[gx[i2]]=pg[gx[i2]]+x[i2];
						}
						for (int i3=0; i3<nra; i3++)
						{
							if (nrag[gx[i3]]>0)
							{
								if (nrag[gx[i3]]<B)  // equation 5
								{
									y[i3]=1-x[i3]+pg[gx[i3]];
								} else {
									y[i3]=1-x[i3]+B*pg[gx[i3]]/(1.0*nrag[gx[i3]]);
								}
							}
						}
						sumy=0;
						for (int i3=0; i3<nra; i3++)
						{
							sumy=sumy+y[i3];
						}
						for (int i3=0; i3<nrg; i3++)
						{
							prevnrag[i3]=nrag[i3];
						}
						// reproduce
						maxp=0;
						for (int i3=0; i3<nra; i3++)
						{
							px[i3]=y[i3]/sumy;
							if (px[i3]>maxp)
							{
								maxp=px[i3];
							}
						}
						
						counter=0;
						while (counter < nra)
						{
							draw=Math.random()*nra;
							number=(int)draw;
							if (Math.random()<(px[number]/maxp))
							{
								xx[counter]=x[number];
								gxx[counter]=gx[number];
								counter=counter+1;
							}
						}					
						for (int i3=0; i3<nra; i3++)					
						{
							x[i3]=xx[i3]+rnd.nextGaussian()*pmut;
							
							if (x[i3]<0)
							{
								x[i3]=0;
							}
							if (x[i3]>1)
							{
								x[i3]=1;
							}

							gx[i3]=gxx[i3];
						}
						for (int i3=0; i3<nra; i3++)
						{
							avgx[gx[i3]]=avgx[gx[i3]]+x[i3];
							avgy[gx[i3]]=avgy[gx[i3]]+y[i3];
						}
						for (int i3=0; i3<nrg; i3++)
						{
							nrag[i3]=0;
						}
						for (int i3=0; i3<nra; i3++)
						{
							nrag[gx[i3]]=nrag[gx[i3]]+1;
						}
						migr=0;
						for (int i3=0; i3<nrg; i3++)
						{
							if (nrag[i3]<prevnrag[i3])
							{
								migr=migr+prevnrag[i3]-nrag[i3];
							}
							if (nrag[i3]>0)
							{
								fw4.write( + avgx[i3]/(nrag[i3]*1.0) + " " + nrag[i3] + " " + (nrag[i3] - prevnrag[i3]) + "\r\n");	
								fw5.write( + avgx[i3]/(nrag[i3]*1.0) + " " + nrag[i3] + " " + (nrag[i3] - prevnrag[i3]) + " ");
						} else {	fw5.write( + 0 + " " + nrag[i3] + " " + (nrag[i3] - prevnrag[i3]) + " ");	
							}
						}
						fw5.write("\r\n");
						maxg=0;
						for (int i2=0; i2<nrg; i2++)
						{
							if (nrag[i2]>0)
							{
								avgx[i2]=avgx[i2]/(nrag[i2]*1.0);
								avgy[i2]=avgy[i2]/(nrag[i2]*1.0);
							}
							if (nrag[i2]>maxg)
							{
								maxg=nrag[i2];
							}
						}
						sumx=0;
						suml=0;
						for (int i3=0; i3<nra; i3++)
						{
							if (Math.random()<mr)
							{
								gx[i3]=rnd.nextInt(nrg);
							}
							sumx=sumx+x[i3];
						}
						if (i1>(totr-1001))
						{
							suma=suma+sumx/(nra*1.0);
							sumyy=sumyy+sumy/(nra*1.0);
							summaxg=summaxg+maxg;
							summigr=summigr+migr;
						}
						fw.write(+ i1 + " " + sumx/(1.0*nra) + " " + maxg + "\r\n");
					}
					fw.write(+ suma/1000.0 + "\r\n");
					sumat=sumat+suma;
					summaxgt=summaxgt+summaxg;
					sumyyt=sumyyt+sumyy;
					summigrt=summigrt+summigr;
				}
				fw2.write(+ mr + " " + pmut + " " + sumat/100000.0 + " " + sumyyt/100000.0 + " " + + summaxgt/100000.0 + " " + summigrt/100000.0 + "\r\n");
			//	fw.write("\r\n");
			}
		}
		fw.close();
		fw2.close();
		fw4.close();
		fw5.close();
	}
      catch (IOException ioe)
      {
      	System.out.println("File-output did not work.\n");
      }
	}
}

class algo implements Parameters
{
}

interface Parameters
{

}
