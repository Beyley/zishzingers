using library 'lbpdeploy';

import 'std:thing';
import 'std:stringa';

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

    pub fn CallNative() {
        let x: void* = 0x10;
        let y: Thing = 0x20 as Thing;
        let z: f32 = 10;
        let w: bool = true;

        NativeFunction(x, y, z, @float4(0, 1, 2, 3), w);
        NativeFunction2();
    }

    pub fn NativeStrCopy() {
        let x: s32* = 0x100;

        @strcpy(x, 'This is a pretty long string copy to test the strcpy code!');
        @strcpy(x, 'This is a pretty long string copy to test the strcpy code!!!');
    }

    pub fn InlineAsm() {
        inline_asm 
        {
        label1:
            NOP
        label2:
            ARG a0, r0 (s32)
            LCsw r8, 'test \' this is a test\n'
            B label1
            BEZ label2, r0
        }
    }

    pub fn PointerArithmatic() {
        let x: s32* = 69;

        x = x + 2;

        let y = x + 10;

        let z: bool* = 0x1234;

        z.* = false;
    }

    pub fn WhileLoop() {
        let count: s32 = 0;
        while(count < 10) {
            count = count + 1;
        }
    }

    @NativeInvoke(0x123456, 0)
    static fn NativeFunction(param1: void*, param2: Thing, param3: f32, param4: vec4, param5: bool) -> void*;

    @NativeInvoke(0x654321, 0)
    static fn NativeFunction2() -> void;
}