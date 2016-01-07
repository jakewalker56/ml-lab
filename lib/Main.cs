using System;
using System.IO;
using System.Text.RegularExpressions;

namespace TransformSPITCAuctionData
{
	class MainClass
	{
		public static void Main (string[] args)
		{
			int counter = 0;

			int eaindex = 5;
			int reaindex = 5;
			int cmaindex = 5;

			string line;

			Regex eargx = new Regex(@"\b\w\w-BEA\d\d\d");
			Regex reargx = new Regex(@"\b\w\w-REA\d\d\d");
			Regex cmargx = new Regex(@"\b\w\w-CMA\d\d\d");

			Regex numrgx = new Regex(@"\d\d\d");
			Regex blockrgx = new Regex(@"-\w\b");
			Regex blockcharrgx = new Regex (@"\w");

			StreamReader file = 
				new StreamReader(@"/Users/jakewalker/Dropbox/SPITC/PE Activity - Financial Info/ceadata/bidsummary.csv");

			StreamWriter ofile = 
				new StreamWriter(@"/Users/jakewalker/Dropbox/SPITC/PE Activity - Financial Info/ceadata/bidsummary_extended.csv");

			line = file.ReadLine (); //first line is special- gives our data format.  Need to add EA, REA, CMA, and Block fields

			line += ",EA,REA,CMA,Block";

			ofile.WriteLine (line);

			while((line = file.ReadLine()) != null)
			{
				int eaval = 0;
				int reaval = 0;
				int cmaval = 0;
				char block = '_';

				//effing commas in the data....
				string[] quotesplit = line.Split('\"');
				for(int i = 1; i < quotesplit.Length; i+=2)
				{
					quotesplit[i] = quotesplit[i].Replace(',', '.');
				}
				string newline = string.Join("\"", quotesplit);

				string[] splits = newline.Split(',');



				MatchCollection eamatches = eargx.Matches(splits[eaindex]);
				MatchCollection reamatches = reargx.Matches(splits[reaindex]);
				MatchCollection cmamatches = cmargx.Matches(splits[cmaindex]);

				if (eamatches.Count > 0)
				{
					eaval = int.Parse(numrgx.Match(eamatches[0].Value).Value);
					block = char.Parse(blockcharrgx.Match (blockrgx.Match(splits[eaindex]).Value).Value);
				}
				else if (reamatches.Count > 0)
				{
					reaval = int.Parse(numrgx.Match(reamatches[0].Value).Value);
					block = char.Parse(blockcharrgx.Match (blockrgx.Match(splits[reaindex]).Value).Value);
				}
				else if (cmamatches.Count > 0)
				{
					cmaval = int.Parse(numrgx.Match(cmamatches[0].Value).Value);
					block = char.Parse(blockcharrgx.Match (blockrgx.Match(splits[cmaindex]).Value).Value);
				}
				else
				{
					block = 'D';
				}

				ofile.WriteLine(line + "," + eaval + "," + reaval + "," + cmaval + "," + block);
				Console.WriteLine(line + "," + eaval + "," + reaval + "," + cmaval + "," + block);

				counter++;
			}
			
			file.Close();
			ofile.Close ();
		}
	}
}
