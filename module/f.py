#---------------#
# import modules
#---------------#
import gzip
import shutil

#---------#
# get data
#--------
def gunzip(file_path,output_path):
    with gzip.open(file_path,"rb") as f_in, open(output_path,"wb") as f_out:
        shutil.copyfileobj(f_in, f_out)

#--------------------------#
# check_difficulty_data.py
#--------------------------#
diff_1_tar = "1d00ffff"

def convert_bits(bits:str) -> hex:
	a = int(bits[:2],16)
	b = int(bits[2:],16)
	return b * 2**(8*(a - 3))


def add_column(csv_filename:str, col_name:str, col_data:list) -> None:
	with open(csv_filename, "r") as read_fp:
		with open(csv_filename.split(".")[0]+"_out.csv","w") as write_fp:
			header = read_fp.readline().strip()
			header += ","+col_name+"\n"
			write_fp.write(header)
			for line, data in zip(read_fp.readlines(),col_data):
				write_fp.write(line.strip()+","+str(data)+"\n")

def get_diff(bits:str) -> float:
	return convert_bits(diff_1_tar)/convert_bits(bits)