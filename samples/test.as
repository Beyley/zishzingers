using library 'lbpdeploy';

import 'std:thing';

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

        NativeFunction(x, y, z, float4(0, 1, 2, 3));
    }

    @NativeInvoke(0x123456, false)
    static fn NativeFunction(param1: void*, param2: Thing, param3: f32, param4: vec4) -> void*;
}