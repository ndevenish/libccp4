        subroutine gdummy
        character *(*) char_dummy
        entry  qreset
          return
        entry  reshap
          return
        entry  qdevic(keybd)
          return
        entry  winope(char_dummy,i0)
          return
        entry  keepas(i1,i2)
          return
        entry  draw2i(i3,i4)
          return
        entry  move2i(i5,i6)
          return
        entry  loadma(i7)
          return
        entry  gconfi
          return
        entry  mmode(i8)
          return
        entry  foregr
          return
        entry  getval(i9)
          return
        entry  color(i10)
          return
        entry  getsiz(r1,r2)
          return
cc        entry  clear this is in somewhere else in -ltermcap
cc          return
        entry  ortho2(r3,r4,r5,r6)
          return
        entry  getori(r7,r8)
          return
        end
