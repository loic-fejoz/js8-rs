use crate::coumpound::Compound;

// submode types
#[derive(PartialEq, Eq, Debug)]
pub enum SubmodeType {
    JS8CallNormal = 0,
    JS8CallFast = 1,
    JS8CallTurbo = 2,
    JS8CallSlow = 4,
    JS8CallUltra = 8,
}

#[derive(PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum FrameType {
    FrameHeartbeatOrCQ = 0b000,
    FrameCoumpound = 0b001,
    FrameCoumpoundDirected = 0b010,
    FrameDirected = 0b011,
    FrameData = 0b100,           // actually 0b10x
    FrameDataCompressed = 0b110, // actually 0b11x
}

#[derive(PartialEq, Debug)]
pub enum Frame {
    FrameHeartbeat {},
    FrameCQ {},
    FrameDirectedMessage {
        from: Option<Compound>,
        to: Option<Compound>,
    },
}

/// frame type transmitted via itype and decoded by the ft8 decoded
#[derive(PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum TransmissionType {
    JS8Call = 0b000,      // <- any other frame of the message
    JS8CallFirst = 0b001, // <- the first frame of a message
    JS8CallLast = 0b010,  // <- the last frame of a message
    JS8CallData = 0b100,  // <- flagged frame (no frame type header)
}

#[derive(PartialEq, Debug)]
pub struct FrameTransmission {
    frame: Frame,
    kind: TransmissionType,
}
