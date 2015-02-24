c23456789112345678921234567893123456789412345678951234567896123456789712
c        ^         ^         ^         ^         ^         ^         ^  
c        10        20        30        40        50        60        70 

c Col.     1: Blank, or c or * or ! for comments
c Col.   2-5: Statement label (optional)
c Col.     6: Continuation of previous line (optional; see below)
c Col.  7-72: Statements
c Col. 73-80: Sequence number (optional, rarely used today)

c      This program reads in a 3 column *.txt file, sets NaN values to
c      -999 and stores the columns into 3 arrays. Data are stored in a 
c      *.txt named by the user. 
       program week1challenge      
       implicit none
       
       integer :: h, i, j, nr, ios , maxrecs
       real :: a,b,c,isNULL=-1,stdA,stdB,stdC,sumsqA,sumsqB,sumsqC,
     + packA(995),packB(997),packC(997) 
c      run once with 1000 to determine number of NaN/-999s then replace.
c      IF number of NaN/-999s in each column are already known then the
c      value nr-number of NaN/-999s should be entered into packA, packB
c      packC respectively.

       character(len=100) :: infile, outfile, header,ax,bx,cx,cont,
     + inputfile
       character(len=1) :: junk
       logical isnanf
       real, dimension(:), allocatable :: arrayA,arrayB,arrayC
       
   
       ! Ask the user for some information about the data to be read in
       write(*,*) "Enter the name of the data file to read..."
       print *
       read(*,*) infile
       
       ! print 1st 10 rows of data
       open (41, file=infile, status='old',action='read')
       print *
       print *, "Printing 1st 10 rows of the file ",infile
       do i=1,10
       read(41,*) ax, bx, cx
       write(*,101) ax, bx, cx
 101   format (a15,8x,a15,8x,a15)   
       end do
       print *
       print *
       ! ask if user id happy to continue
       write(*,*) "Continue (y/n)? ..."
       print *
       read(*,*) cont
       print *
       
       if(cont.eq.'y') goto 42
       if(cont.eq.'n') goto 99
       
 42    close(41) !close 1st 10 print
       
       !open user defined data file to be read
       open (11, file=infile, status='old',action='read')
       
       !Ask user if any heading lines of text exist
      write(*,*) "Does data file contain headings (y/n)? ..."
      print *
      read(*,*) header
      print *
      
      if(header.eq.'y') goto 25
      if(header.eq.'n') goto 75

       !Ask user if the first n lines should be skipped
 25    write(*,*) "how many lines should be skipped? ..."
       print *
       read(*,*) h
       print *
       
       do 50 i=1,h !Skip the 1st h rows (containing headers)
       read (11, *)
 50    continue  
 
       ! Ask the user to name the output file
 75    write(*,*) "Enter the name of the output file to be created..."
       print *
       read(*,*) outfile
       open (21,file=outfile,action='write')
       
c       infile='week1data.txt'
c       outfile='week1data_mung.txt'  
       
       !read in data and set NaN values to -999
       ! detect NaN in pgifortran = isnanf()
       ! detect NaN in gfortran = isnan()
       do 
              read (11, *,end=100) a, b, c
              if (isnanf(a)) then
              a=-999
c              elseif (a.eq.-999) then
c              a = "*"
              end if
              if (isnanf(b)) then
              b=-999
c              elseif (b.eq.-999) then
c              b = "*"
              end if
              if (isnanf(c)) then
              c=-999
c              elseif (c.eq.-999) then
c              c = "*"
              end if
c              if (b.eq.-999) then
c              b=sqrt(isNULL)
c              end if
c              if (c.eq.-999) then
c              c=sqrt(isNULL)
c              end if
              write (21,*) a, b, c   !write out data to file
       end do
       
 100   close (11)
       close (21)
       print *
       write(*,*) "... WORKING ..."
       print *
       write(*,*) "... ... ... ..."
       write(*,*) "... ... ... ..."
       write(*,*) "... ... ... ..."
       print *
       print *
       write(*,*) "Output writen to:", outfile
       print *  
       print *, "Storing output into distinct arrays ..."
       print *
       ! Ask the user for some information about the data to be read in
c       write(*,*) "Enter name of file to read in..."
       inputfile=outfile
c       read(*,*) inputfile
       
       write(*,*) "Enter max number of records..."
       write(*,*) "IF max number of records UNKNOWN then enter 10000..."
       print *
       read(*,*) maxrecs
       
       !Determine total number of lines in file
       nr = 0
       open(1,file=inputfile)
       do j=1,maxrecs
       read(1,*,iostat=ios) junk
       if (ios /= 0) exit
       if (j == maxrecs) then
       write(*,*) "Error: Maximum number of records exceeded..."
       write(*,*) "INCREASE maximum record number and re-run..."
       write(*,*) "Exiting program now..."
       stop
       end if
       nr = nr + 1
       end do
       rewind(1)
       
       !Now we can allocate data variables
c       write(*,*) "Number or records in data file = ", nr
       allocate(arrayA(nr))
       allocate(arrayB(nr))
       allocate(arrayC(nr))
       
       !Now read data into 3 seperate nr x 1 arrays
       do j=1,nr
       read(1,*) arrayA(J),arrayB(J), arrayC(J)

       end do
       close(1)
       
c       open (69, file='week1dataARRAY.txt')
c       do j=1,nr
c       write(69,*) arrayA(J),arrayB(J), arrayC(J)
c       write(*,*) arrayA(J),arrayB(J), arrayC(J)
c       end do
c       close(69)
       

c       Where (arrayA.gt.0)
c       arrayAA = arrayA
c       end where

       packA=pack(arrayA,arrayA/=-999)
       packB=pack(arrayB,arrayB/=-999)
       packC=pack(arrayC,arrayC/=-999)
              
       sumsqA= sum(packA*packA)
       stdA=sqrt(sumsqA/995 - (sum(packA)/995)**2)
       sumsqB= sum(packB*packB)
       stdB=sqrt(sumsqB/997 - (sum(packB)/997)**2)
       sumsqC= sum(packC*packC)
       stdC=sqrt(sumsqC/997 - (sum(packC)/997)**2)
       print *
       write(*,*) "---------------------------------------------"
       write(*,*) "-          Desciptive statistics            -"
       write(*,*) "---------------------------------------------"
       write(*,*) 'Data      No. Recs  Mean            Std Dev. '
       write(*,*) 'Apples    995   ', sum(packA)/max(1,size(packA)),
     +   stdA
       write(*,*) 'Oranges   997   ', sum(packB)/max(1,size(packB)),
     +   stdB
       write(*,*) 'Control   997   ', sum(packC)/max(1,size(packC)),
     +   stdC
       write(*,*) "---------------------------------------------"
       print *
       write(*,*) "PROGRAMME ENDED"
       write(*,*) "No errors encountered"

       
 99    end program week1challenge
