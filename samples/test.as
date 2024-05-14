using library 'lbpdeploy';

class Test(g1234568)
{
    pub fn AssignmentTest() {
        let x: s32 = 0;
        let y: s32 = 1;

        y = x = 2;
    }

    pub fn BitwiseOrder() {
        let x: s32 = 0;

        if(x & 2 != 0) {

        }
    }

    pub fn Cast() {
        let x: s32 = 0;
        let y: f32 = x as f32;
    }

    pub fn Ptr() {
        let x: s32** = 0xc;

        let y = x.*;

        x.* = y;
    }
}